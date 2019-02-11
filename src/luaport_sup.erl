-module(luaport_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([spawn/6]).
-export([respawn/1]).
-export([despawn/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 1, 5}, []}}.

spawn(Id, Path, Args, M, Pipe, Timeout) ->
  supervisor:start_child(?MODULE, {Id, {luaport_server, start_link, [Id, Path, Args, M, Pipe, Timeout]}, transient, 1000, worker, [luaport_server]}).

respawn(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:restart_child(?MODULE, Id).

despawn(Id) ->
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).
