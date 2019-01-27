# LuaPort
An erlang port for scripting application logic in lua
```
{ok, Pid} = luaport:spawn(some_id, "path/to/scripts"),
{ok, Results} = luaport:call(Pid, some_lua_function, [arg1, 2, "hello?", <<"bye!">>, [], {}, #{}]),
luaport:despawn(some_id).
```

## Test
```
git clone https://github.com/asciibeats/luaport.git
cd luaport
rebar3 ct
```
