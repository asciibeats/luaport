-module(luaport_SUITE).

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
  Path = filename:join([code:priv_dir(luaport), lua]),
  {ok, Pid} = luaport:spawn(banane, Path, ?MODULE),
  {ok, [[128]]} = luaport:call(Pid, echo, [[128]]),
  {ok, [-2147483648, 2147483647]} = luaport:call(Pid, echo, [-2147483648, 2147483647]),
  {error, "don't panic"} = luaport:call(Pid, error, [<<"don't panic">>, 0]),
  luaport:cast(Pid, 'after', [300, first]),
  luaport:cast(Pid, 'after', [100, second]),
  {ok, [LRef]} = luaport:call(Pid, 'after', [500, third]),
  luaport:cast(Pid, exec, [cancel, LRef]),
  luaport:cast(Pid, interval, [200]),
  {ok, [[{true, false}]]} = luaport:call(Pid, echo, [[{true, false, undefined}]]),
  {ok, [{[<<"other">>, "abc"]}]} = luaport:call(Pid, echo, [{[other, "abc"]}]),
  {ok, [#{<<"one">> := 1,[2,3.7,4] := #{}}]} = luaport:call(Pid, echo, [#{one => 1, [2,3.7,4] => #{}}]),
  {ok, [{{2, 3}, {<<"vier">>, 5}}]} = luaport:call(Pid, echo, [{{2, 3}, {<<"vier">>, 5}}]),
  {ok, [6]} = luaport:call(Pid, call, [<<"multiply">>, 2, 3]),
  {ok, []} = luaport:call(Pid, call, [<<"undefined">>, 2, 3]),
  PortRef2 = {local, moin},
  {ok, _Pid2} = luaport:spawn(PortRef2, Path, ?MODULE, [thing]),
  {ok, [15]} = luaport:call(PortRef2, call, [<<"multiply">>, 3, 5]),
  ok = luaport:despawn(PortRef2),
  PortRef3 = {global, sven},
  {ok, _Pid3} = luaport:spawn(PortRef3, Path, ?MODULE),
  {ok, _Pid4} = luaport:respawn(PortRef3),
  luaport:cast(PortRef3, print, [<<"done">>]),
  ok = luaport:despawn(PortRef3),
  {ok, [true]} = luaport:call(Pid, exec, [islist, "abc"]),
  {ok, [true]} = luaport:call(Pid, exec, [istuple, {}]),
  {ok, [true]} = luaport:call(Pid, exec, [ismap, #{}]),
  {ok, [false]} = luaport:call(Pid, exec, [islist, 7]),
  {ok, [false]} = luaport:call(Pid, exec, [istuple, true]),
  {ok, [false]} = luaport:call(Pid, exec, [ismap, false]),
  {ok, [[]]} = luaport:call(Pid, exec, [aslist, {}]),
  {ok, [{}]} = luaport:call(Pid, exec, [astuple, []]),
  {ok, [#{1 := 97, 2 := 98, 3 := 99}]} = luaport:call(Pid, exec, [asmap, "abc"]),
  {ok, [<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++">>]} = luaport:call(Pid, echo, [<<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++">>]),
  ok = luaport:despawn(banane),
  application:stop(luaport).

init(_PortRef, Name) ->
  [42, Name].
init(_PortRef, thing, Name) ->
  [23, Name].

multiply(_PortRef, A, B) ->
  [A * B].
multiply(_PortRef, thing, A, B) ->
  [A * B].

print(PortRef, A) ->
  ct:pal("print: ~p ~p~n", [PortRef, A]).
print(PortRef, thing, A) ->
  ct:pal("print: ~p ~p~n", [PortRef, A]).
