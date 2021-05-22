# LuaPort
*An [erlang/elixir port](http://erlang.org/doc/tutorial/c_port.html) for scripting application logic in lua. Works with [lua](https://www.lua.org) and [luajit](https://luajit.org).*

Use erlang...
```erlang
{ok, Pid, []} = luaport:spawn(some_id, "path/to/scripts"),
{ok, [6]} = luaport:call(Pid, 'Multiply', [2, 3]).
```
...or elixir...
```elixir
{:ok, pid, []} = :luaport.spawn(:some_id, 'path/to/scripts')
{:ok, [6]} = :luaport.call(pid, :Multiply, [2, 3])
```
...to execute a lua script:
```lua
function Multiply(a, b)
  return a * b
end
```

## Test
```
git clone https://github.com/asciibeats/luaport.git
cd luaport
rebar3 ct
```

## Use
If you use erlang and [rebar3](https://www.rebar3.org), add LuaPort as dependency to your `rebar.config`.
```erlang
{deps, [
  {luaport, "1.5.0"}
]}.
```
Or for elixir and mix, add it to your `mix.exs`.
```elixir
defp deps do
  [
    {:luaport, "~> 1.5"}
  ]
end
```
Create a lua script at path/to/scripts called `main.lua`.
```lua
function Subtract(a, b)
  return a - b
end
```
When using erlang, don't forget to start the application.
```erlang
application:start(luaport),
{ok, Pid, []} = luaport:spawn(myid, "path/to/scripts"),
{ok, [42]} = luaport:call(Pid, 'Subtract', [43, 1]),
luaport:despawn(myid),
application:stop(luaport).
```
With elixir it will start automatically.
```elixir
{:ok, pid, []} = :luaport.spawn(:myid, 'path/to/scripts')
{:ok, [42]} = :luaport.call(pid, :Subtract, [43, 1])
:luaport.despawn(:myid)
```
To return results on spawn and respawn, just add a return statement to your `main.lua`...
```lua
function Something()
  print('do something')
end

return 23, 42
```
...and retrieve them like this:
```erlang
{ok, Pid1, [23, 42]} = luaport:spawn(myid, "path/to/scripts"),
{ok, Pid2, [23, 42]} = luaport:respawn(myid).
```
To add some static data to lua's context, add a map as third argument to the spawn function.
```erlang
{ok, Pid, []} = luaport:spawn(myid, "path/to/scripts", #{config => {what, ever}, greeting => <<"moin">>}).
```
The elements of that map will be available as global variables. Be careful not to choose colliding names, as these variables will be named after the maps keys.
```lua
local a, b = unpack(config)

function Greet()
  print(greeting)
end
```
To push data into the global context of a running port, use the push function.
```erlang
luaport:push(myid, #{name => <<"til">>}).
```
To pull some dynamic data into lua's context, you may provide a callback module as the fourth argument.
```erlang
{ok, Pid, []} = luaport:spawn(myid, "path/to/scripts", #{}, callback).
```
Calls and casts will automagically be mapped to the module's function of the same name.
```lua
local map, number = port.call.init('sunshine', 49)
port.cast.print('some message')
```
The port's reference is the first argument of every callback.
```erlang
-module(callback).

-export([init/3, print/2]).

init(myid, String, Number) ->
  [#{string => String}, Number].

print(myid, Message) ->
  io:format("Message: ~p~n", [Message]).
```
If you want to insert or just execute some code during runtime, use the load function.
```erlang
{ok, []} = luaport:load(Pid, <<"function something() return 666 end">>),
{ok, [666]} = luaport:call(Pid, something).
```
```erlang
{ok, []} = luaport:load(Pid, <<"print('nice')">>).
```
To be able to continuously call or cast functions after accidental or intended respawns, you could use `{global, Name}` or `{local, Name}` as reference to register the port.
```erlang
{ok, _Pid1, []} = luaport:spawn({local, myid}, "path/to/scripts"),
{ok, _Pid2, []} = luaport:respawn({local, myid}),
luaport:cast({local, myid}, 'Execute').
```
Requiring modules works normally. You may put a module.lua or module.so into path/to/scripts or any other path in lua's package.path or package.cpath, respectively.
```lua
local module = require('module')
```
Lua has no delayed call mechanism, therefore LuaPort provides an interface to erlangs timer functions. The first argument is the time to wait in milliseconds.
```lua
port.after(3000, function (str) print(str) end, 'call once, if not canceled')
```
```lua
local ref = port.interval(1000, function (str) print(str) end, 'call repeatedly until canceled')
port.cancel(ref)
```
Finally, to just suspend execution for a while, there exists a sleep function.
```lua
port.sleep(2000)
```

## Quirks
Since erlang and lua datatypes do not align too nicely, there are some things to consider.

- Lua has only one collection type, the table. It is like a map in erlang. So when maps get translated to lua they become tables.
- When lists or tuples get translated they become tables with a metatype 'list' or 'tuple', respectively.
- Strings in erlang are lists and translated as such. Lua has no dedicated binary type. If you want to translate to strings, use binary strings.
- Erlang has no boolean type and atoms serve no purpose in lua context. So atom 'true' translates to true and atom 'false' to false.
- Atom 'nil' translates to nil.
- For convenience, all other atoms become strings. They will be handled like any other string on their way back.
- If compiled to use [LuaJIT](https://luajit.org), LuaPort has no integer type. By default, numbers that are [almost integers](c_src/luaport.c#L56-L64) get converted. You can modify this behaviour by defining `LUAP_NOINT` on compilation, disabling integer handling.
- LuaPort uses a custom print function mimicking Lua's own print function. It differs in a few ways: It shows its output in the erlang shell, prints tables in depth and can take a variable number of arguments.

#### Translations
| Erlang | Elixir | Lua | Notes |
| --- | --- | --- | --- |
| 23 | 23 | 23.0 | |
| "abc" | 'abc' | {97, 98, 99} | erlang strings are lists |
| <<"abc">> | "abc" | 'abc' | |
| \[1, 2] | \[1, 2] | {1, 2} | has metatype 'list' |
| {3, 4} | {3, 4} | {3, 4} | has metatype 'tuple' |
| #{5 => 6} | %{5 => 6} | {\[5] = 6} | has no metatype |
| true | true | true | |
| false | false | false | |
| nil | nil | nil | |
| atom | :atom | 'atom' | |

#### Helpers
| Function | Description |
| --- | --- |
| port.aslist(t) | set metatype 'list' |
| port.astuple(t) | set metatype 'tuple' |
| port.asmap(t) | unset metatype |
| port.islist(v) | if metatype 'list' |
| port.istuple(v) | if metatype 'tuple' |
| port.ismap(v) | if no metatype |

## Notes
-Apologies for the occasional bad commit discipline/hygiene.
-Thank you!