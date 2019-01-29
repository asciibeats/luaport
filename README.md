# LuaPort
*An erlang port for scripting application logic in lua*

Supported datatypes are: Numbers, Strings, Binaries, Atoms, Lists, Tuples and Maps.
```erlang
{ok, Pid} = luaport:spawn(some_id, "path/to/scripts"),
{ok, Results} = luaport:call(Pid, multiply, [2, 3]),
luaport:despawn(some_id).
```
```lua
function multiply(a, b)
  return a * b
end
```
Luaports can also be spawned with a callback module to be able to call erlang functions from lua context.
```erlang
{ok, Pid} = luaport:spawn(42, "path/to/scripts", callback),
{ok, Results} = luaport:call(Pid, execute),
luaport:despawn(42).
```
```lua
function execute()
  local results = luaport.call.divide(3, 2)
end
```
```erlang
-module(callback).

-export([divide/2]).

divide(A, B) ->
  [A / B].
```

## Test
```
git clone https://github.com/asciibeats/luaport.git
cd luaport
rebar3 ct
```

## Use
I presume you use [rebar3](https://www.rebar3.org). Just add luaport as dependency to your *rebar.config*.
```erlang
{deps, [
  {luaport, {git, "https://github.com/asciibeats/luaport.git", {branch, "master"}}}
]}.
```
Create a lua script at *path/to/scripts* called *main.lua*.
```lua
function subtract(a, b)
  return a - b
end
```
Don't forget to start the application before you use it.
```erlang
application:start(luaport),
{ok, Pid} = luaport:spawn("myid", "path/to/scripts"),
{ok, Results} = luaport:call(Pid, subtract, [43, 1]),
luaport:despawn("myid"),
application:stop(luaport).
```
Be happy!

## Quirks
Since erlang and lua datatypes do not align too nicely, there are some things to consider. Strings in erlang are lists and handled as such. If you want to send strings, use binary strings.

| Erlang | Lua |
| --- | --- |
| <<"string">> | 'string' |
| [1, 2] | {1, 2} --with metatype 'list' |
| {3, 4} | {3, 4} --with metatype 'tuple' |
| #{2 => 4} | {[2] = 4} --with metatype 'map' |
| true | true |
| false | false |
| other_atom | userdata 'atom' |

The following applies when compiled with LUAP_UNDEFINED_AS_NIL set.

| Erlang | Lua |
| --- | --- |
| undefined | nil |

There are several lua functions to help convert inbetween.

| Function | Description |
| --- | --- |
| aslist(t) | set metatype 'list' |
| astuple(t) | set metatype 'tuple' |
| asmap(t) | set metatype 'map' |
| toatom(s) | string to atom |
| tostring(a) | atom to string |

Also there exist some testfunctions since maps, lists and tuples are really tables in lua and have the same lua type `type(t) == 'table'`. Otherwise they could not be used in lua's standard functions.

| Function |
| --- |
| islist(v) |
| istuple(v) |
| ismap(v) |
| isatom(v) |
