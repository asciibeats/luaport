-module(luaport_server).
-behaviour(gen_server).

-export([start_link/3]).
-export([stop/1]).
-export([call/4]).
-export([cast/3]).

% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(EXIT_STATUS, #{199 => bad_main, 200 => bad_version, 201 => bad_tuple, 202 => bad_atom, 203 => bad_args, 204 => bad_func, 205 => bad_string, 206 => wrong_arity, 207 => wrong_status, 208 => fail_read, 209 => fail_write, 210 => fail_alloc}).

-record(state, {id, callback, port}).

start_link(Id, Path, Callback) ->
	gen_server:start_link({global, Id}, ?MODULE, {Id, Path, Callback}, []).

stop(Pid) ->
	gen_server:stop(Pid, shutdown, 3000).

call(Pid, Name, Args, Timeout) when is_atom(Name), is_list(Args) ->
	gen_server:call(Pid, {call, Name, Args}, Timeout).

cast(Pid, Name, Args) when is_atom(Name), is_list(Args) ->
	gen_server:cast(Pid, {cast, Name, Args}).

% gen_server
init({Id, Path, Callback}) ->
	process_flag(trap_exit, true),
	Exec = filename:join([code:priv_dir(luaport), "luaport"]),
	Port = open_port({spawn_executable, Exec}, [{cd, Path}, {packet, 4}, binary, exit_status]),
	{ok, #state{id = Id, callback = Callback, port = Port}}.

handle_call({call, Name, Args}, _From, #state{port = Port} = State) ->
	Port ! {self(), {command, term_to_binary({Name, Args})}},
	{reply, receive_port(State), State}.

handle_cast({cast, Name, Args}, #state{port = Port} = State) ->
	Port ! {self(), {command, term_to_binary({Name, Args})}},
	{ok, _} = receive_port(State),
	{noreply, State}.

handle_info({_Port, {exit_status, Code}}, State) ->
	{stop, maps:get(Code, ?EXIT_STATUS, Code), State};
handle_info({'EXIT', _Port, Reason}, State) ->
	{stop, Reason, State}.

terminate(shutdown, #state{port = Port} = _State) ->
	case port_close(Port) of
		true -> ok
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

receive_port(#state{port = Port} = State) ->
	receive 
		{Port, {data, Data}} -> 
			case binary_to_term(Data, [safe]) of
				{call, {Func, Args}} ->
					Port ! {self(), {command, term_to_binary({apply(State#state.callback, Func, Args)})}},
					receive_port(State);
				{cast, {Func, Args}} ->
					apply(State#state.callback, Func, Args),
					receive_port(State);
				{info, List} -> 
					io:format("~ninf ~p ~p", [Port, List]),
					receive_port(State);
				{error, Reason} ->
					io:format("~nerr ~p ~p", [Port, Reason]),
					{error, Reason};
				{ok, Results} ->
					{ok, Results}
			end
	end.
