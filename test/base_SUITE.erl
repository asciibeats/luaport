-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
 
all() -> [case1].

case1(_Config) ->
	ok = application:start(luaport),
	Path = filename:join([code:priv_dir(luaport), modes, base]),
	{ok, Pid} = luaport:spawn(<<"name">>, Path),
	{error, <<"main.lua:2: attempt to index a nil value (global 'a')">>} = luaport:call(Pid, fail),
	{error, <<"main.lua:6: some error message">>} = luaport:call(Pid, callerror, [<<"some error message">>, 1]),
	{ok, [[]]} = luaport:call(Pid, echo, [[]]),
	{ok, [#{}]} = luaport:call(Pid, echo, [#{}]),
	{ok, [{}]} = luaport:call(Pid, echo, [{}]),
	{ok, [[<<"nil">>, 2]]} = luaport:call(Pid, echo, [[nil, 2]]),
	{ok, [#{0 := 1, 1 := 2}]} = luaport:call(Pid, echo, [#{0 => 1, 1 => 2}]),
	{ok, [{2, <<"nil">>}]} = luaport:call(Pid, echo, [{2, nil}]),
	{ok, [#{<<"a">> := [], <<"b">> := []}]} = luaport:call(Pid, echo, [#{<<"a">> => [], <<"b">> => []}]),
	{ok, [6]} = luaport:call(Pid, echo, [6]),
	{ok, [6.0]} = luaport:call(Pid, echo, [6.0]),
	ok = luaport:despawn(<<"name">>),
	ok = application:stop(luaport).
