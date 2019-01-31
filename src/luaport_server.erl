-module(luaport_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([stop/1]).
-export([call/6]).
-export([cast/5]).

% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(EXIT_STATUS, #{199 => bad_main, 200 => bad_version, 201 => bad_tuple, 202 => bad_atom, 203 => bad_args, 204 => bad_func, 205 => bad_string, 206 => wrong_arity, 207 => wrong_status, 208 => fail_read, 209 => fail_write, 210 => fail_alloc}).

start_link(Path) ->
  gen_server:start_link(?MODULE, Path, []).

stop(Pid) ->
  gen_server:stop(Pid, shutdown, 3000).

call(Pid, Name, Args, Timeout, Callback, Pipe) when is_atom(Name), is_list(Args), is_list(Pipe) ->
  gen_server:call(Pid, {call, Name, Args, Callback, Pipe}, Timeout).

cast(Pid, Name, Args, Callback, Pipe) when is_atom(Name), is_list(Args), is_atom(Callback), is_list(Pipe) ->
  gen_server:cast(Pid, {cast, Name, Args, Callback, Pipe}).

% gen_server
init(Path) ->
  process_flag(trap_exit, true),
  Exec = filename:join([code:priv_dir(luaport), "luaport"]),
  Port = open_port({spawn_executable, Exec}, [{cd, Path}, {packet, 4}, binary, exit_status]),
  {ok, Port}.

handle_call({call, Name, Args, Callback, Pipe}, _From, Port) ->
  Port ! {self(), {command, term_to_binary({Name, Args})}},
  {reply, receive_port(Port, Callback, Pipe), Port}.

handle_cast({cast, Name, Args, Callback, Pipe}, Port) ->
  Port ! {self(), {command, term_to_binary({Name, Args})}},
  receive_port(Port, Callback, Pipe),
  {noreply, Port}.

handle_info({_Port, {exit_status, Code}}, Port) ->
  {stop, maps:get(Code, ?EXIT_STATUS, Code), Port};
handle_info({'EXIT', _Port, Reason}, Port) ->
  {stop, Reason, Port}.

terminate(shutdown, Port) ->
  case port_close(Port) of
    true -> ok
  end.

code_change(_OldVsn, Port, _Extra) ->
  {ok, Port}.

receive_port(Port, Callback, Pipe) ->
  receive 
    {Port, {data, Data}} -> 
      case binary_to_term(Data, [safe]) of
        {call, {Func, Args}} when Callback =/= undefined ->
          Port ! {self(), {command, term_to_binary({apply(Callback, Func, Pipe ++ Args)})}},
          receive_port(Port, Callback, Pipe);
        {call, _} ->
          {error, callback_undefined};
        {cast, {Func, Args}} when Callback =/= undefined ->
          apply(Callback, Func, Pipe ++ Args),
          receive_port(Port, Callback, Pipe);
        {cast, _} ->
          {error, callback_undefined};
        {info, List} -> 
          io:format("~ninf ~p ~p", [Port, List]),
          receive_port(Port, Callback, Pipe);
        {error, Reason} ->
          io:format("~nerr ~p ~p", [Port, Reason]),
          {error, Reason};
        {ok, Results} ->
          {ok, Results}
      end
  end.
