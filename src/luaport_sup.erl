-module(luaport_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([spawn/2]).
-export([respawn/1]).
-export([despawn/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 10, 10}, []}}.

spawn(Id, Path) ->
  supervisor:start_child(?MODULE, {Id, {luaport_server, start_link, [Path]}, permanent, 5000, worker, [luaport_server]}).

respawn(Id) ->
  supervisor:restart_child(?MODULE, Id).

despawn(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).
