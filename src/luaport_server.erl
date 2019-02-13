-module(luaport_server).

-export([start_link/5]).
-export([init/5]).
-export([call/4]).
-export([cast/4]).

-define(TIMEOUT, 5000).
-define(RESTART, 200).

-define(EXIT_REASONS, #{
  0 => {shutdown, success},
  139 => {respawn, segmentation_fault},
  141 => {respawn, broken_pipe},

  200 => {respawn, fail_read},
  201 => {respawn, fail_write},

  210 => {respawn, bad_version},
  211 => {respawn, bad_command},
  212 => {respawn, bad_atom},
  213 => {respawn, bad_func},
  214 => {respawn, bad_args},

  220 => {respawn, call_read},
  221 => {respawn, call_version},
  222 => {respawn, call_result}}).

start_link(Id, Path, M, Pipe, Timeout) ->
  {ok, spawn_link(?MODULE, init, [Id, Path, M, Pipe, Timeout])}.

init(Id, Path, M, Pipe, Timeout) when is_list(Path), is_atom(M), is_list(Pipe) ->
  process_flag(trap_exit, true),
  Exec = filename:join([code:priv_dir(luaport), "luaport"]),
  Port = open_port({spawn_executable, Exec}, [{cd, Path}, {packet, 4}, binary, exit_status]),
  {ok, []} = portloop(Id, Port, M, Pipe, Timeout),
  mainloop(Id, Port, M, Pipe).

call(Pid, F, A, Timeout) when is_atom(F), is_list(A) ->
  Ref = make_ref(),
  Pid ! {call, self(), Ref, F, A, Timeout},
  receive
    {Ref, Result} -> Result
  end.

cast(Pid, F, A, Timeout) when is_atom(F), is_list(A) ->
  Pid ! {cast, F, A, Timeout},
  ok.

mainloop(Id, Port, M, Pipe) ->
  receive
    {call, From, Ref, F, A, Timeout} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      From ! {Ref, portloop(Id, Port, M, Pipe, Timeout)},
      mainloop(Id, Port, M, Pipe);
    {cast, F, A, Timeout} ->
      Port ! {self(), {command, term_to_binary({F, A})}},
      portloop(Id, Port, M, Pipe, Timeout),
      mainloop(Id, Port, M, Pipe);
    {'EXIT', _From, Reason} ->
      port_close(Port),
      exit(Reason)
  end.

portloop(Id, Port, M, Pipe, Timeout) ->
  receive
    {Port, {data, Data}} ->
      try binary_to_term(Data, [safe]) of
        {call, F, A} when M =/= undefined ->
          Result = tryapply(M, F, [Id | Pipe ++ A]),
          Port ! {self(), {command, term_to_binary(Result)}},
          portloop(Id, Port, M, Pipe, Timeout);
        {call, _, _} ->
          Port ! {self(), {command, term_to_binary([])}},
          portloop(Id, Port, M, Pipe, Timeout);
        {cast, F, A} when M =/= undefined ->
          tryapply(M, F, [Id | Pipe ++ A]),
          portloop(Id, Port, M, Pipe, Timeout);
        {cast, _, _} ->
          portloop(Id, Port, M, Pipe, Timeout);
        {info, List} -> 
          io:format("inf ~p ~p~n", [Id, List]),
          portloop(Id, Port, M, Pipe, Timeout);
        {error, Reason} ->
          io:format("err ~p ~p~n", [Id, Reason]),
          {error, Reason};
        {ok, Results} ->
          {ok, Results}
      catch
        error:badarg -> exit({respawn, unsafe_data})
      end;
    {Port, {exit_status, Status}} ->
      exit(maps:get(Status, ?EXIT_REASONS, {respawn, Status}))
  after Timeout ->
    io:format("err ~p ~p~n", [Id, timeout]),
    {error, timeout}
  end.

tryapply(M, F, A) ->
  try
    apply(M, F, A)
  catch
    error:undef -> []
  end.
