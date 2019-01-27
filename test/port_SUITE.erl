-module(port_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
-export([test_call/1]).
-export([test_cast/1]).
 
all() -> [case1].

case1(_Config) ->
	ok = application:start(luaport),
	Path = filename:join([code:priv_dir(luaport), "modes", "port"]),
	{ok, Pid} = luaport:spawn(<<"name">>, Path, ?MODULE),
	%check if global table 'state' exists
	{ok, [#{}]} = luaport:call(Pid, getstate),
	%check if global function 'print' is replace with port.info
	{ok, []} = luaport:call(Pid, callprint, ["this message was send by calling print()"]),
	%port.call
	{ok, [<<"ö">>]} = luaport:call(Pid, call, [test_call, <<"ö">>]),
	{ok, [<<"ö"/utf8>>]} = luaport:call(Pid, call, [test_call, <<"ö"/utf8>>]),
	%port.cast
	{ok, []} = luaport:call(Pid, cast, [test_cast, foo]),
	%port.info
	{ok, []} = luaport:call(Pid, info, ["info", {1, 1.0, <<"string">>, #{}, [], {}}]),
	%port.asmap
	{ok, [#{1 := 1, 2 := <<"a">>, 3 := <<"t">>}]} = luaport:call(Pid, asmap, [[1, a, <<"t">>]]),
	%port.aslist
	{ok, [[]]} = luaport:call(Pid, aslist, [#{2 => 2, 4 => 4}]),
	{ok, [[2]]} = luaport:call(Pid, aslist, [#{1 => 2, 4 => 4}]),
	{ok, [[2, 4]]} = luaport:call(Pid, aslist, [#{1 => 2, 2 => 4}]),
	%port.astuple
	{ok, [{}]} = luaport:call(Pid, astuple, [#{2 => 2, 4 => 4}]),
	{ok, [{2}]} = luaport:call(Pid, astuple, [#{1 => 2, 4 => 4}]),
	{ok, [{2, 4}]} = luaport:call(Pid, astuple, [#{1 => 2, 2 => 4}]),
	%port.astuple
	{ok, [TupleList]} = luaport:call(Pid, astuplelist, [#{2 => 2, <<"a">> => 4, true => 42}]),
	case lists:keysort(1, TupleList) of
		[{2, 2}, {true, 42}, {<<"a">>, 4}] -> ok
	end,
	ok = luaport:despawn(<<"name">>),
	ok = application:stop(luaport).

test_call(A) ->
	ct:pal("Cast: ~n~p~n", [A]),
	[A].

test_cast(A) ->
	ct:pal("Cast: ~n~p~n", [A]).
