-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
-export([init/2]).
-export([init/3]).
-export([multiply/3]).
-export([multiply/4]).
-export([print/2]).
-export([print/3]).

all() -> [case1].

case1(_Config) ->
  application:start(luaport),
  Path = filename:join([code:priv_dir(luaport), modes, base]),
  {ok, Pid} = luaport:spawn(banane, Path, ?MODULE),
  {ok, [9, 8]} = luaport:call(Pid, echo, [9, 8]),
  {error, "don't panic"} = luaport:call(Pid, error, [<<"don't panic">>, 0]),
  {ok, [[{true, false}]]} = luaport:call(Pid, echo, [[{true, false, undefined}]]),
  {ok, [{[<<"other">>, "abc"]}]} = luaport:call(Pid, echo, [{[other, "abc"]}]),
  {ok, [#{<<"one">> := 1,[2,3.7,4] := #{}}]} = luaport:call(Pid, echo, [#{one => 1, [2,3.7,4] => #{}}]),
  {ok, [{{2, 3}, {<<"vier">>, 5}}]} = luaport:call(Pid, echo, [{{2, 3}, {<<"vier">>, 5}}]),
  {ok, [6]} = luaport:call(Pid, call, [<<"multiply">>, 2, 3]),
  {ok, []} = luaport:call(Pid, call, [<<"undefined">>, 2, 3]),
  {ok, Pid2} = luaport:spawn(42, Path, ?MODULE, [thing]),
  {ok, [15]} = luaport:call(Pid2, call, [<<"multiply">>, 3, 5]),
  ok = luaport:despawn(42),
  {ok, _Pid3} = luaport:spawn(42, Path, ?MODULE),
  {ok, Pid4} = luaport:respawn(42),
  {ok, []} = luaport:call(Pid4, print, [<<"done">>]),
  ok = luaport:despawn(42),
  {ok, [true]} = luaport:call(Pid, exec, [<<"islist">>, "abc"]),
  {ok, [true]} = luaport:call(Pid, exec, [<<"istuple">>, {}]),
  {ok, [true]} = luaport:call(Pid, exec, [<<"ismap">>, #{}]),
  {ok, [false]} = luaport:call(Pid, exec, [<<"islist">>, 7]),
  {ok, [false]} = luaport:call(Pid, exec, [<<"istuple">>, true]),
  {ok, [false]} = luaport:call(Pid, exec, [<<"ismap">>, false]),
  {ok, [[]]} = luaport:call(Pid, exec, [<<"aslist">>, {}]),
  {ok, [{}]} = luaport:call(Pid, exec, [<<"astuple">>, []]),
  {ok, [#{1 := 97, 2 := 98, 3 := 99}]} = luaport:call(Pid, exec, [<<"asmap">>, "abc"]),
  ok = luaport:despawn(banane),
  application:stop(luaport).

init(_Id, Name) ->
  [42, Name].
init(_Id, thing, Name) ->
  [23, Name].

multiply(_Id, A, B) ->
  [A * B].
multiply(_Id, thing, A, B) ->
  [A * B].

print(Id, A) ->
  ct:pal("print: ~p ~p~n", [Id, A]).
print(Id, thing, A) ->
  ct:pal("print: ~p ~p~n", [Id, A]).
