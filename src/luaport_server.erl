-module(luaport_server).

-export([start_link/5]).
-export([init/5]).
-export([call/4]).
-export([cast/4]).
-export([load/3]).
-export([push/3]).

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
  218 => bad_config,
  219 => bad_any,
  220 => call_read,
  221 => call_version,
  222 => call_result,
  230 => print_len,
  231 => print_malloc,
  232 => print_buf,
  240 => ei_malloc}).

start_link(PortRef, Path, Config, Callback, Timeout) ->
  Pid = spawn_link(?MODULE, init, [PortRef, Path, Config, Callback, Timeout]),
  register_name(PortRef, Pid),
  Ref = make_ref(),
  Pid ! {results, self(), Ref},
  receive {Ref, Results} -> {ok, Pid, Results} end.

init(PortRef, Path, Config, Callback, Timeout) when is_list(Path), is_map(Config), is_atom(Callback) ->
  Exec = filename:join([code:priv_dir(luaport), "luaport"]),
  Port = open_port({spawn_executable, Exec}, [{cd, Path}, {packet, 4}, binary, exit_status]),
  Port ! {self(), {command, term_to_binary(Config)}},
  {TRefs, {ok, Results}} = portloop(PortRef, Port, Callback, #{}, Timeout),
  mainloop(PortRef, Port, Callback, TRefs, Results).

call(PortRef, F, A, Timeout) when is_atom(F), is_list(A) ->
  Ref = make_ref(),
  send(PortRef, {call, F, A, Timeout, self(), Ref}),
  receive {Ref, Result} -> Result end.

cast(PortRef, F, A, Timeout) when is_atom(F), is_list(A) ->
  send(PortRef, {cast, F, A, Timeout}), ok.

load(PortRef, Binary, Timeout) when is_binary(Binary) ->
  Ref = make_ref(),
  send(PortRef, {load, Binary, Timeout, self(), Ref}),
  receive {Ref, Result} -> Result end.

push(PortRef, Global, Timeout) when is_map(Global) ->
  send(PortRef, {push, Global, Timeout}), ok.

mainloop(PortRef, Port, Callback, TRefs, Results) ->
  receive
    {call, F, A, Timeout, From, Ref} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      case portloop(PortRef, Port, Callback, TRefs, Timeout) of
        {error, Reason} ->
          From ! {Ref, {error, Reason}},
          port_close(Port),
          exit(Reason);
        {NewTRefs, Result} ->
          From ! {Ref, Result},
          mainloop(PortRef, Port, Callback, NewTRefs, Results)
      end;
    {cast, F, A, Timeout} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      case portloop(PortRef, Port, Callback, TRefs, Timeout) of
        {error, Reason} ->
          port_close(Port),
          exit(Reason);
        {NewTRefs, _Result} ->
          mainloop(PortRef, Port, Callback, NewTRefs, Results)
      end;
    {load, Binary, Timeout, From, Ref} ->
      Port ! {self(), {command, term_to_binary(Binary)}},
      case portloop(PortRef, Port, Callback, TRefs, Timeout) of
        {error, Reason} ->
          From ! {Ref, {error, Reason}},
          port_close(Port),
          exit(Reason);
        {NewTRefs, Result} ->
          From ! {Ref, Result},
          mainloop(PortRef, Port, Callback, NewTRefs, Results)
      end;
    {push, Global, Timeout} ->
      Port ! {self(), {command, term_to_binary(Global)}},
      case portloop(PortRef, Port, Callback, TRefs, Timeout) of
        {error, Reason} ->
          port_close(Port),
          exit(Reason);
        {NewTRefs, _Result} ->
          mainloop(PortRef, Port, Callback, NewTRefs, Results)
      end;
    {'after', LRef, Timeout} ->
      case maps:take(LRef, TRefs) of
        {_TRef, NewTRefs} ->
          Port ! {self(), {command, term_to_binary(LRef)}},
          {NewerTRefs, _Result} = portloop(PortRef, Port, Callback, NewTRefs, Timeout),
          mainloop(PortRef, Port, Callback, NewerTRefs, Results);
        error ->
          mainloop(PortRef, Port, Callback, TRefs, Results)
      end;
    {interval, LRef, Timeout} ->
      case maps:is_key(LRef, TRefs) of
        true ->
          Port ! {self(), {command, term_to_binary(-LRef)}},
          {NewTRefs, _Result} = portloop(PortRef, Port, Callback, TRefs, Timeout),
          mainloop(PortRef, Port, Callback, NewTRefs, Results);
        false ->
          mainloop(PortRef, Port, Callback, TRefs, Results)
      end;
    {results, From, Ref} ->
      From ! {Ref, Results},
      mainloop(PortRef, Port, Callback, TRefs, Results)
  end.

portloop(PortRef, Port, Callback, TRefs, Timeout) ->
  receive
    {Port, {data, Data}} ->
      try binary_to_term(Data, [safe]) of
        {call, F, A} ->
          Result = tryapply(Callback, F, [PortRef | A]),
          Port ! {self(), {command, term_to_binary(Result)}},
          portloop(PortRef, Port, Callback, TRefs, Timeout);
        {cast, F, A} ->
          tryapply(Callback, F, [PortRef | A]),
          portloop(PortRef, Port, Callback, TRefs, Timeout);
        {info, List} ->
          io:format("lua ~w ~p~n", [PortRef, List]),
          portloop(PortRef, Port, Callback, TRefs, Timeout);
        {'after', Time, LRef} ->
          {ok, TRef} = timer:send_after(Time, {'after', LRef, Timeout}),
          NewTRefs = maps:put(LRef, TRef, TRefs),
          portloop(PortRef, Port, Callback, NewTRefs, Timeout);
        {interval, Time, LRef} ->
          {ok, TRef} = timer:send_interval(Time, {interval, LRef, Timeout}),
          NewTRefs = maps:put(LRef, TRef, TRefs),
          portloop(PortRef, Port, Callback, NewTRefs, Timeout);
        {cancel, LRef} ->
          case maps:take(LRef, TRefs) of
            {TRef, NewTRefs} ->
              timer:cancel(TRef),
              portloop(PortRef, Port, Callback, NewTRefs, Timeout);
            error ->
              portloop(PortRef, Port, Callback, TRefs, Timeout)
          end;
        {error, Reason} ->
          io:format("err ~w ~s~n", [PortRef, Reason]),
          {TRefs, {error, Reason}};
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

tryapply(Callback, F, A) when Callback =/= undefined ->
  try
    apply(Callback, F, A)
  catch
    error:undef -> []
  end;
tryapply(_Callback, _F, _A) ->
  [].
