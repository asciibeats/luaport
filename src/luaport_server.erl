-module(luaport_server).

-export([start_link/5]).
-export([init/5]).
-export([call/5]).
-export([cast/5]).
-export([load/3]).

-define(EXIT_REASONS, #{
  0 => success,
  139 => segmentation_fault,
  141 => broken_pipe,
  200 => fail_read,
  201 => fail_write,
  202 => fail_size,
  203 => fail_jit,
  210 => bad_version,
  211 => bad_tuple,
  212 => bad_atom,
  213 => bad_func,
  214 => bad_args,
  215 => bad_call,
  216 => bad_binary,
  217 => bad_command,
  220 => call_read,
  221 => call_version,
  222 => call_result,
  230 => print_len,
  231 => print_malloc,
  232 => print_buf,
  240 => ei_malloc}).

start_link(PortRef, Path, M, Pipe, Timeout) ->
  Pid = spawn_link(?MODULE, init, [PortRef, Path, M, Pipe, Timeout]),
  register_name(PortRef, Pid),
  {ok, Pid}.

init(PortRef, Path, M, Pipe, Timeout) when is_list(Path), is_atom(M), is_list(Pipe) ->
  Exec = filename:join([code:priv_dir(luaport), "luaport"]),
  Port = open_port({spawn_executable, Exec}, [{cd, Path}, {packet, 4}, binary, exit_status]),
  {TRefs, {ok, []}} = portloop(PortRef, Port, M, Pipe, #{}, Timeout),
  mainloop(PortRef, Port, M, Pipe, TRefs).

call(PortRef, F, A, Pipe, Timeout) when is_atom(F), is_list(A), is_list(Pipe) ->
  Ref = make_ref(),
  send(PortRef, {call, F, A, Pipe, Timeout, self(), Ref}),
  receive
    {Ref, Result} -> Result
  end.

cast(PortRef, F, A, Pipe, Timeout) when is_atom(F), is_list(A), is_list(Pipe) ->
  send(PortRef, {cast, F, A, Pipe, Timeout}),
  ok.

load(PortRef, Binary, Timeout) when is_binary(Binary) ->
  Ref = make_ref(),
  send(PortRef, {load, Binary, Timeout, self(), Ref}),
  receive
    {Ref, Result} -> Result
  end.

mainloop(PortRef, Port, M, Pipe, TRefs) ->
  receive
    {call, F, A, Pipe2, Timeout, From, Ref} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      case portloop(PortRef, Port, M, Pipe ++ Pipe2, TRefs, Timeout) of
        {error, Reason} ->
          From ! {Ref, {error, Reason}},
          port_close(Port),
          exit(Reason);
        {NewTRefs, Result} ->
          From ! {Ref, Result},
          mainloop(PortRef, Port, M, Pipe, NewTRefs)
      end;
    {cast, F, A, Pipe2, Timeout} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      case portloop(PortRef, Port, M, Pipe ++ Pipe2, TRefs, Timeout) of
        {error, Reason} ->
          port_close(Port),
          exit(Reason);
        {NewTRefs, _Result} ->
          mainloop(PortRef, Port, M, Pipe, NewTRefs)
      end;
    {load, Binary, Timeout, From, Ref} ->
      Port ! {self(), {command, term_to_binary(Binary)}},
      case portloop(PortRef, Port, M, Pipe, TRefs, Timeout) of
        {error, Reason} ->
          From ! {Ref, {error, Reason}},
          port_close(Port),
          exit(Reason);
        {NewTRefs, Result} ->
          From ! {Ref, Result},
          mainloop(PortRef, Port, M, Pipe, NewTRefs)
      end;
    {'after', LRef, Timeout} ->
      case maps:take(LRef, TRefs) of
        {_TRef, NewTRefs} ->
          Port ! {self(), {command, term_to_binary(LRef)}},
          {NewerTRefs, _Result} = portloop(PortRef, Port, M, Pipe, NewTRefs, Timeout),
          mainloop(PortRef, Port, M, Pipe, NewerTRefs);
        error ->
          mainloop(PortRef, Port, M, Pipe, TRefs)
      end;
    {interval, LRef, Timeout} ->
      case maps:is_key(LRef, TRefs) of
        true ->
          Port ! {self(), {command, term_to_binary(-LRef)}},
          {NewTRefs, _Result} = portloop(PortRef, Port, M, Pipe, TRefs, Timeout),
          mainloop(PortRef, Port, M, Pipe, NewTRefs);
        false ->
          mainloop(PortRef, Port, M, Pipe, TRefs)
      end
  end.

portloop(PortRef, Port, M, Pipe, TRefs, Timeout) ->
  receive
    {Port, {data, Data}} ->
      try binary_to_term(Data, [safe]) of
        {call, F, A} ->
          Result = tryapply(M, F, [PortRef | Pipe ++ A]),
          Port ! {self(), {command, term_to_binary(Result)}},
          portloop(PortRef, Port, M, Pipe, TRefs, Timeout);
        {cast, F, A} ->
          tryapply(M, F, [PortRef | Pipe ++ A]),
          portloop(PortRef, Port, M, Pipe, TRefs, Timeout);
        {info, List} ->
          io:format("lua ~w ~p~n", [PortRef, List]),
          portloop(PortRef, Port, M, Pipe, TRefs, Timeout);
        {'after', Time, LRef} ->
          {ok, TRef} = timer:send_after(Time, {'after', LRef, Timeout}),
          NewTRefs = maps:put(LRef, TRef, TRefs),
          portloop(PortRef, Port, M, Pipe, NewTRefs, Timeout);
        {interval, Time, LRef} ->
          {ok, TRef} = timer:send_interval(Time, {interval, LRef, Timeout}),
          NewTRefs = maps:put(LRef, TRef, TRefs),
          portloop(PortRef, Port, M, Pipe, NewTRefs, Timeout);
        {cancel, LRef} ->
          case maps:take(LRef, TRefs) of
            {TRef, NewTRefs} ->
              timer:cancel(TRef),
              portloop(PortRef, Port, M, Pipe, NewTRefs, Timeout);
            error ->
              portloop(PortRef, Port, M, Pipe, TRefs, Timeout)
          end;
        {error, Reason} ->
          io:format("err ~w ~s~n", [PortRef, Reason]),
          {TRefs, {error, {lua, Reason}}};
        {ok, Results} ->
          {TRefs, {ok, Results}}
      catch
        error:badarg -> {error, unsafe_data}
      end;
    {Port, {exit_status, Status}} ->
      {error, {port, maps:get(Status, ?EXIT_REASONS, Status)}}
  after Timeout ->
    io:format("err ~w ~w~n", [PortRef, timeout]),
    {error, timeout}
  end.

register_name({global, Name}, Pid) ->
  global:register_name(Name, Pid),
  wait_for_name(Name);
register_name({local, Name}, Pid) ->
  register(Name, Pid);
register_name(_PortRef, _Pid) ->
  ok.

send({global, Name}, Message) ->
  global:send(Name, Message);
send({local, Name}, Message) ->
  Name ! Message;
send(Pid, Message) ->
  Pid ! Message.

wait_for_name(Name) ->
  wait_for_name(Name, false).
wait_for_name(Name, false) ->
  timer:sleep(500),
  wait_for_name(Name, lists:member(Name, global:registered_names()));
wait_for_name(_Name, true) ->
  ok.

tryapply(M, F, A) when M =/= undefined ->
  try
    apply(M, F, A)
  catch
    error:undef -> []
  end;
tryapply(_M, _F, _A) ->
  [].
