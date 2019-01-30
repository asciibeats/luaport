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
  %spawn without a callback
  {ok, Pid1} = luaport:spawn(42, Path),
  {error, callback_undefined} = luaport:call(Pid1, call, [<<"test_call">>, <<"hallo">>]),
  %spawn with a callback
  {ok, Pid2} = luaport:spawn(<<"name">>, Path, ?MODULE),
  %if global table 'state' exists
  {ok, [#{}]} = luaport:call(Pid2, getstate),
  %if nil is translated to undefined
  {ok, [undefined]} = luaport:call(Pid2, getnil),
  %port.call
  {ok, [<<"ö">>]} = luaport:call(Pid2, call, [<<"test_call">>, <<"ö">>]),
  {ok, [<<"ö"/utf8>>]} = luaport:call(Pid2, call, [<<"test_call">>, <<"ö"/utf8>>]),
  %port.cast
  {ok, []} = luaport:call(Pid2, cast, [<<"test_cast">>, "foo"]),
  %custom print replacement
  {ok, []} = luaport:call(Pid2, info, ["info", {1, 1.0, <<"string">>, #{}, [], {}}]),
  %port.ismap
  {ok, [true]} = luaport:call(Pid2, ismap, [#{}]),
  {ok, [false]} = luaport:call(Pid2, ismap, [[]]),
  {ok, [false]} = luaport:call(Pid2, ismap, [12]),
  %port.islist
  {ok, [true]} = luaport:call(Pid2, islist, [[]]),
  {ok, [false]} = luaport:call(Pid2, islist, [#{}]),
  %port.istuple
  {ok, [true]} = luaport:call(Pid2, istuple, [{}]),
  {ok, [false]} = luaport:call(Pid2, istuple, [#{}]),
  %port.asmap
  {ok, [#{1 := 1, 2 := "a", 3 := <<"t">>}]} = luaport:call(Pid2, asmap, [[1, "a", <<"t">>]]),
  %port.aslist
  {ok, [[]]} = luaport:call(Pid2, aslist, [#{2 => 2, 4 => 4}]),
  {ok, [[2]]} = luaport:call(Pid2, aslist, [#{1 => 2, 4 => 4}]),
  {ok, [[2, 4]]} = luaport:call(Pid2, aslist, [#{1 => 2, 2 => 4}]),
  %port.astuple
  {ok, [{}]} = luaport:call(Pid2, astuple, [#{2 => 2, 4 => 4}]),
  {ok, [{2}]} = luaport:call(Pid2, astuple, [#{1 => 2, 4 => 4}]),
  {ok, [{2, 4}]} = luaport:call(Pid2, astuple, [#{1 => 2, 2 => 4}]),
  ok = luaport:despawn(<<"name">>),
  ok = luaport:despawn(42),
  ok = application:stop(luaport).

test_call(A) ->
  [A].

test_cast(_A) ->
  ok.
