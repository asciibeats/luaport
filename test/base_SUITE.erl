-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
-export([test_call/7]).
-export([test_cast/1]).

-define(TIMEOUT, 1000).
 
all() -> [case1].

case1(_Config) ->
  ok = application:start(luaport),
  Path = filename:join([code:priv_dir(luaport), modes, base]),
  {ok, Pid} = luaport:spawn(<<"name">>, Path),
  {ok, ["hallo", "welt"]} = luaport:call(Pid, echo, ["hallo", "welt"]),
  {error, <<"don't panic">>} = luaport:call(Pid, error, [<<"don't panic">>, 0]),
  {ok, [[{true, false, false}]]} = luaport:call(Pid, echo, [[{true, false, other}]]),
  {ok, [#{<<"one">> := 1,[2,3.7,4] := #{}}]} = luaport:call(Pid, echo, [#{<<"one">> => 1, [2,3.7,4] => #{}}]),
  {ok, [{{2, 3}, {<<"vier">>, 5}}]} = luaport:call(Pid, echo, [{{2, 3}, {<<"vier">>, 5}}]),
  {ok, Pid1} = luaport:spawn(42, Path),
  {error, callback_undefined} = luaport:call(Pid1, call, [<<"test_call">>, <<"hallo">>,  <<"welt">>, atom, 12, #{}], ?TIMEOUT, undefined, [[], {}]),
  %{ok, Pid3} = luaport:respawn(42),
  %{ok, [#{}, undefined]} = luaport:call(Pid3, getstate),
  {ok, Pid2} = luaport:spawn(myid, Path),
  {ok, [<<"ö">>, <<"ö"/utf8>>]} = luaport:call(Pid2, call, [<<"test_call">>, <<"ö">>, <<"ö"/utf8>>, atom, 12, #{}], ?TIMEOUT, ?MODULE, [[], {}]),
  ok = luaport:cast(Pid2, call, [<<"test_call">>, <<"ö">>, <<"ö"/utf8>>, atom, 12, #{}], ?MODULE, [[], {}]),
  {ok, [#{}, undefined]} = luaport:call(Pid2, getstate),
  {ok, []} = luaport:call(Pid2, cast, [<<"test_cast">>, true], ?TIMEOUT, ?MODULE),
  ok = luaport:cast(Pid2, cast, [<<"test_cast">>, true], ?MODULE),
  {ok, [true]} = luaport:call(Pid2, execute, [<<"islist">>, "asdf"]),
  {ok, [true]} = luaport:call(Pid2, execute, [<<"istuple">>, {}]),
  {ok, [true]} = luaport:call(Pid2, execute, [<<"ismap">>, #{}]),
  {ok, [false]} = luaport:call(Pid2, execute, [<<"islist">>, 7]),
  {ok, [false]} = luaport:call(Pid2, execute, [<<"istuple">>, true]),
  {ok, [false]} = luaport:call(Pid2, execute, [<<"ismap">>, false]),
  {ok, [[]]} = luaport:call(Pid2, execute, [<<"aslist">>, {}]),
  {ok, [{}]} = luaport:call(Pid2, execute, [<<"astuple">>, []]),
  {ok, [#{1 := 97}]} = luaport:call(Pid2, execute, [<<"asmap">>, "a"]),
  ok = luaport:despawn(myid),
  ok = luaport:despawn(<<"name">>),
  ok = luaport:despawn(42),
  ok = application:stop(luaport).

test_call([], {}, A, B, false, 12, #{}) ->
  [A, B].

test_cast(_A) ->
  ok.
