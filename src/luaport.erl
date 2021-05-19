-module(luaport).

-export([spawn/2, spawn/3, spawn/4, spawn/5]).
-export([respawn/1]).
-export([despawn/1]).
-export([call/2, call/3, call/4]).
-export([cast/2, cast/3, cast/4]).
-export([load/2, load/3]).

spawn(PortRef, Path) ->
  ?MODULE:spawn(PortRef, Path, #{}).
spawn(PortRef, Path, Config) ->
  ?MODULE:spawn(PortRef, Path, Config, undefined).
spawn(PortRef, Path, Config, Callback) ->
  ?MODULE:spawn(PortRef, Path, Config, Callback, 1000).
spawn(PortRef, Path, Config, Callback, Timeout) ->
  luaport_sup:spawn(PortRef, Path, Config, Callback, Timeout).

respawn(PortRef) ->
  luaport_sup:respawn(PortRef).

despawn(PortRef) ->
  luaport_sup:despawn(PortRef).

call(PortRef, F) ->
  call(PortRef, F, []).
call(PortRef, F, A) ->
  call(PortRef, F, A, 1000).
call(PortRef, F, A, Timeout) ->
  luaport_server:call(PortRef, F, A, Timeout).

cast(PortRef, F) ->
 cast(PortRef, F, []).
cast(PortRef, F, A) ->
 cast(PortRef, F, A, 1000).
cast(PortRef, F, A, Timeout) ->
 luaport_server:cast(PortRef, F, A, Timeout).

load(PortRef, Binary) ->
 load(PortRef, Binary, 1000).
load(PortRef, Binary, Timeout) ->
 luaport_server:load(PortRef, Binary, Timeout).
