-module(luaport).

-export([spawn/2, spawn/3, spawn/4, spawn/5]).
-export([respawn/1]).
-export([despawn/1]).
-export([call/2, call/3, call/4]).
-export([cast/2, cast/3, cast/4]).

spawn(Id, Path) ->
  ?MODULE:spawn(Id, Path, undefined).
spawn(Id, Path, M) ->
  ?MODULE:spawn(Id, Path, M, []).
spawn(Id, Path, M, Pipe) ->
  ?MODULE:spawn(Id, Path, M, Pipe, 1000).
spawn(Id, Path, M, Pipe, Timeout) ->
  luaport_sup:spawn(Id, Path, M, Pipe, Timeout).

respawn(Id) ->
  luaport_sup:respawn(Id).

despawn(Id) ->
  luaport_sup:despawn(Id).

call(Pid, F) ->
  call(Pid, F, []).
call(Pid, F, A) ->
  call(Pid, F, A, 1000).
call(Pid, F, A, Timeout) ->
  luaport_server:call(Pid, F, A, Timeout).

cast(Pid, F) ->
 cast(Pid, F, []).
cast(Pid, F, A) ->
 cast(Pid, F, A, 1000).
cast(Pid, F, A, Timeout) ->
 luaport_server:cast(Pid, F, A, Timeout).
