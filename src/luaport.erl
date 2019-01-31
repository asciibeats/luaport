-module(luaport).

-export([spawn/2]).
-export([respawn/1]).
-export([despawn/1]).
-export([call/2]).
-export([call/3]).
-export([call/4]).
-export([call/5]).
-export([call/6]).
-export([cast/2]).
-export([cast/3]).
-export([cast/4]).
-export([cast/5]).

-define(TIMEOUT, 1000).

spawn(Id, Path) ->
  luaport_sup:spawn(Id, Path).

respawn(Id) ->
  luaport_sup:respawn(Id).

despawn(Id) ->
  luaport_sup:despawn(Id).

call(Pid, Name) ->
  call(Pid, Name, []).
call(Pid, Name, Args) ->
  call(Pid, Name, Args, ?TIMEOUT).
call(Pid, Name, Args, Timeout) ->
  call(Pid, Name, Args, Timeout, undefined).
call(Pid, Name, Args, Timeout, Callback) ->
  call(Pid, Name, Args, Timeout, Callback, []).
call(Pid, Name, Args, Timeout, Callback, Pipe) ->
  luaport_server:call(Pid, Name, Args, Timeout, Callback, Pipe).

cast(Pid, Name) ->
 cast(Pid, Name, []).
cast(Pid, Name, Args) ->
 cast(Pid, Name, Args, undefined).
cast(Pid, Name, Args, Callback) ->
 cast(Pid, Name, Args, Callback, []).
cast(Pid, Name, Args, Callback, Pipe) ->
 luaport_server:cast(Pid, Name, Args, Callback, Pipe).
