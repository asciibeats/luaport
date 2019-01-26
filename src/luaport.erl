-module(luaport).

-export([spawn/3]).
-export([respawn/1]).
-export([despawn/1]).
-export([call/2]).
-export([call/3]).
-export([call/4]).
%-export([cast/2]).
%-export([cast/3]).

-define(TIMEOUT, 1000).

spawn(Id, Path, Callback) ->
	luaport_sup:spawn(Id, Path, Callback).

respawn(Id) ->
	luaport_sup:respawn(Id).

despawn(Id) ->
	luaport_sup:despawn(Id).

call(Pid, Name) ->
	call(Pid, Name, [], ?TIMEOUT).
call(Pid, Name, Args) ->
	call(Pid, Name, Args, ?TIMEOUT).
call(Pid, Name, Args, Timeout) ->
	luaport_server:call(Pid, Name, Args, Timeout).

cast(Pid, Name) ->
	cast(Pid, Name, []).
cast(Pid, Name, Args) ->
	luaport_server:cast(Pid, Name, Args).
