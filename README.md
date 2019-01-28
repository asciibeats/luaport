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
  local results = luaport.call('divide', 3, 2)
end
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
Don't forget to start the application before you use it.
```erlang
application:start(luaport),
{ok, Pid} = luaport:spawn(myport, "path/to/scripts"),
luaport:call(Pid, multiply, [23, 42]),
luaport:despawn(myport),
application:stop(luaport).
```
