-module(luaport_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([case1/1]).
-export([init/1]).
-export([multiply/2]).
-export([print/1]).

all() ->
  [case1].

case1(_Config) ->
  application:start(luaport),
  Path = filename:join([code:priv_dir(luaport), lua]),
  {ok, Pid, []} = luaport:spawn(banane, Path, #{config => {666, <<"moin">>}}, ?MODULE),
  {ok, []} = luaport:load(Pid, <<"print(_VERSION)">>),
  {ok, [[128]]} = luaport:call(Pid, 'Echo', [[128]]),
  ok = luaport:push(Pid, #{asdf => {999, qwer}}),
  {ok, [{666, <<"moin">>}, {999, <<"qwer">>}]} = luaport:load(Pid, <<"return config, asdf">>),
  {ok, [-2147483648, 2147483647]} = luaport:call(Pid, 'Echo', [-2147483648, 2147483647]),
  {error, "don't panic"} = luaport:call(Pid, error, [<<"don't panic">>, 0]),
  luaport:cast(Pid, 'After', [300, first]),
  luaport:cast(Pid, 'After', [100, second]),
  {ok, [LRef]} = luaport:call(Pid, 'After', [500, third]),
  luaport:cast(Pid, 'Exec', [cancel, LRef]),
  luaport:cast(Pid, 'Interval', [200]),
  {ok, [#{}, 1]} = luaport:call(Pid, 'Echo', [#{}, 1]),
  {ok, [[{<<"atom">>, true, false}, "abc"]]} = luaport:call(Pid, 'Echo', [[{atom, true, false, nil}, "abc"]]),
  %{ok, [[{atom, true, false, nil}, "abc"]]} = luaport:call(Pid, 'Echo', [[{atom, true, false, nil}, "abc"]]),
  {ok, [{{2, 3}, {<<"vier">>, 5}}]} = luaport:call(Pid, 'Echo', [{{2, 3}, {<<"vier">>, 5}}]),
  {ok, [6]} = luaport:call(Pid, 'Call', [<<"multiply">>, 2, 3]),
  {ok, []} = luaport:call(Pid, 'Call', [<<"undefined">>, 2, 3]),
  PortRef2 = {local, moin},
  {ok, _Pid2, []} = luaport:spawn(PortRef2, Path, #{}, ?MODULE),
  {ok, [15]} = luaport:call(PortRef2, 'Call', [<<"multiply">>, 3, 5]),
  ok = luaport:despawn(PortRef2),
  PortRef3 = {global, sven},
  {ok, _Pid3, []} = luaport:spawn(PortRef3, Path, #{}, ?MODULE),
  {ok, _Pid4, []} = luaport:respawn(PortRef3),
  luaport:cast(PortRef3, print, [<<"done">>]),
  ok = luaport:despawn(PortRef3),
  luaport:cast(Pid, print, [<<"sleeping for two seconds...">>]),
  luaport:call(Pid, 'Sleep', [2000], 3000),
  luaport:cast(Pid, print, [<<"...done">>]),
  {ok, [true]} = luaport:call(Pid, 'Exec', [islist, "abc"]),
  {ok, [true]} = luaport:call(Pid, 'Exec', [ismap, #{}]),
  {ok, [false]} = luaport:call(Pid, 'Exec', [islist, 7]),
  {ok, [false]} = luaport:call(Pid, 'Exec', [ismap, false]),
  {ok, [[]]} = luaport:call(Pid, 'Exec', [aslist, #{}]),
  {ok, [#{1 := 97, 2 := 98, 3 := 99}]} = luaport:call(Pid, 'Exec', [asmap, "abc"]),
  {ok, [true]} = luaport:call(Pid, 'Exec', [istuple, {}]),
  {ok, [false]} = luaport:call(Pid, 'Exec', [istuple, true]),
  {ok, [{}]} = luaport:call(Pid, 'Exec', [astuple, []]),
  {ok, [<<"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "+++++++++++++++">>]} = luaport:call(Pid, 'Echo',
       [<<"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
          "+++++++++++++++">>]),
  ok = luaport:despawn(banane),
  NilRef = {global, nil},
  NilPath = filename:join([code:priv_dir(luaport), nil]),
  {ok, _Pid, [3, <<"50">>]} = luaport:spawn(NilRef, NilPath),
  {ok, Pid5, [3, <<"50">>]} = luaport:respawn(NilRef),
  {ok, Binary1} = file:read_file(filename:join([NilPath, "load1.lua"])),
  {ok, []} = luaport:load(Pid5, Binary1),
  luaport:cast(Pid5, 'Func1'),
  {ok, Binary2} = file:read_file(filename:join([NilPath, "load2.lua"])),
  {ok, []} = luaport:load(Pid5, Binary2),
  luaport:cast(Pid5, 'Func2'),
  luaport:cast(Pid5, 'Func1'),
  {error, _Reason} = luaport:load(Pid5, <<"invalid">>),
  {ok, []} = luaport:load(Pid5, <<"print('nice')">>),
  {ok, []} = luaport:load(Pid5, <<"a = 42">>),
  {ok, [42]} = luaport:load(Pid5, <<"return a">>),
  {ok, []} = luaport:load(Pid5, <<"a = nil">>),
  {ok, [nil]} = luaport:load(Pid5, <<"return a">>),
  {ok, []} = luaport:load(Pid5, <<"function something() return 666 end">>),
  {ok, [666]} = luaport:load(Pid5, <<"return something()">>),
  ok = luaport:despawn(NilRef),
  application:stop(luaport).

init(Name) ->
  [42, Name].

multiply(A, B) ->
  [A * B].

print(A) ->
  ct:pal("print: ~p~n", [A]).