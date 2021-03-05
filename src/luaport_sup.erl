-module(luaport_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([stop/0]).
-export([init/1]).
-export([spawn/5]).
-export([respawn/1]).
-export([despawn/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  supervisor:stop({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 1, 5}, []}}.

spawn(PortRef, Path, M, Pipe, Timeout) ->
  supervisor:start_child(?MODULE, {PortRef, {luaport_server, start_link, [PortRef, Path, M, Pipe, Timeout]}, transient, 1000, worker, [luaport_server]}).

respawn(PortRef) ->
  supervisor:terminate_child(?MODULE, PortRef),
  supervisor:restart_child(?MODULE, PortRef).

despawn(PortRef) ->
  supervisor:terminate_child(?MODULE, PortRef),
  supervisor:delete_child(?MODULE, PortRef).
