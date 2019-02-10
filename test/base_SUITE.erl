-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
-export([multiply/3]).
-export([print/2]).

-define(TIMEOUT, 1000).
 
all() -> [case1].

case1(_Config) ->
  ct:pal("~n"),
  application:start(luaport),
  Path = filename:join([code:priv_dir(luaport), modes, base]),
  {ok, Pid} = luaport:spawn(banane, Path, #{<<"seed">> => 23}, ?MODULE),
  {error, <<"don't panic">>} = luaport:call(Pid, error, [<<"don't panic">>, 0]),
  {ok, [[{true, false, false}]]} = luaport:call(Pid, echo, [[{true, false, other}]]),
  {ok, [#{<<"one">> := 1,[2,3.7,4] := #{}}]} = luaport:call(Pid, echo, [#{<<"one">> => 1, [2,3.7,4] => #{}}]),
  {ok, [{{2, 3}, {<<"vier">>, 5}}]} = luaport:call(Pid, echo, [{{2, 3}, {<<"vier">>, 5}}]),
  {ok, [6]} = luaport:call(Pid, call, [<<"multiply">>, 2, 3]),
  {ok, [15]} = luaport:call(Pid, call, [<<"multiply">>, 3, 5]),
  {ok, Pid2} = luaport:spawn(42, Path, #{<<"id">> => 42}, ?MODULE),
  luaport:cast(Pid2, call, [<<"multiply">>, 2, 3]),
  luaport:cast(Pid2, cast, [<<"multiply">>, 3, 5]),
  luaport:despawn(42),
  {ok, Pid3} = luaport:spawn(42, Path, #{<<"set">> => <<"abc">>}),
  {ok, []} = luaport:call(Pid3, call, [<<"multiply">>, 2, 3]),
  {ok, []} = luaport:call(Pid3, print, [<<"done">>]),
  luaport:despawn(42),
  {ok, Pid4} = luaport:spawn(42, Path, #{<<"set">> => <<"def">>}, ?MODULE, []),
  {ok, []} = luaport:call(Pid4, cast, [<<"print">>, <<"ghi">>]),
  luaport:despawn(42),
  luaport:cast(Pid, cast, [<<"multiply">>, 2, 3]),
  {ok, []} = luaport:call(Pid, cast, [<<"multiply">>, 3, 5]),
  {ok, [true]} = luaport:call(Pid, execute, [<<"islist">>, "abc"]),
  {ok, [true]} = luaport:call(Pid, execute, [<<"istuple">>, {}]),
  {ok, [true]} = luaport:call(Pid, execute, [<<"ismap">>, #{}]),
  {ok, [false]} = luaport:call(Pid, execute, [<<"islist">>, 7]),
  {ok, [false]} = luaport:call(Pid, execute, [<<"istuple">>, true]),
  {ok, [false]} = luaport:call(Pid, execute, [<<"ismap">>, false]),
  {ok, [[]]} = luaport:call(Pid, execute, [<<"aslist">>, {}]),
  {ok, [{}]} = luaport:call(Pid, execute, [<<"astuple">>, []]),
  {ok, [#{1 := 97, 2 := 98, 3 := 99}]} = luaport:call(Pid, execute, [<<"asmap">>, "abc"]),
  luaport:despawn(banane),
  application:stop(luaport).

multiply(Id, A, B) ->
  [A * B].

print(Id, A) ->
  ct:pal("print: ~p~n", [A]).
