# LuaPort
An erlang port for scripting application logic in lua. Supported datatypes are: Numbers, Strings, Binaries, Atoms, Lists, Tuples and Maps.
```
{ok, Pid} = luaport:spawn(some_id, "path/to/scripts"),
{ok, Results} = luaport:call(Pid, multiply, [2, 3]),
luaport:despawn(some_id).
```
```
function multiply(a, b)
	return a * b
end
```
Luaports can also be spawned with a callback module to be able to call erlang functions from the lua context.
```
{ok, Pid} = luaport:spawn(42, "path/to/scripts", callback),
{ok, Results} = luaport:call(Pid, execute),
luaport:despawn(42).
```
```
function execute()
	local results =  luaport.call(devide, 3, 2)
end
```

## Test
```
git clone https://github.com/asciibeats/luaport.git
cd luaport
rebar3 ct
```
