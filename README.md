# LuaPort
*An [erlang port](http://erlang.org/doc/tutorial/c_port.html) for scripting application logic in lua*
```erlang
{ok, Pid} = luaport:spawn(someid, "path/to/scripts"),
{ok, [6]} = luaport:call(Pid, multiply, [2, 3]),
```
```lua
function multiply(a, b)
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
I presume you use [rebar3](https://www.rebar3.org). Just add LuaPort as dependency to your rebar.config.
```erlang
{deps, [
  {luaport, {git, "https://github.com/asciibeats/luaport.git", {branch, "master"}}}
]}.
```
Create a lua script at path/to/scripts called main.lua.
```lua
function subtract(a, b)
  return a - b
end
```
Add an init function, if you want to do something on spawn or respawn. The arguments are the third parameter of spawn.
```lua
function init(...)
  print(...)
end
```
Don't forget to start the application before you use it.
```erlang
application:start(luaport),
{ok, Pid} = luaport:spawn(myid, "path/to/scripts", [#{<<"really?">> => true}]),
{ok, Results} = luaport:call(Pid, subtract, [43, 1]),
luaport:despawn(myid),
application:stop(luaport).
```
Ports can also be spawned with a callback module to be able to call or cast erlang functions from lua context. The second list, the pipe, is not interpreted by the port. Its elements will become arguments when calling or casting back.
```erlang
{ok, Pid} = luaport:spawn(myid, "path/to/scripts", [], callback, ["abc", 23]),
luaport:cast(Pid, execute),
```
```lua
function execute()
  local result = luaport.call.divide(3, 2)
  luaport.cast.divide(3, 2)
end
```
The port's id is the first argument of every callback, followed by the pipe and the original arguments.
```erlang
-module(callback).

-export([divide/5]).

divide(myid, "abc", 23, A, B) ->
  [A / B].
```
Requiring modules works normally. You can put a module.lua or module.so into path/to/scripts or any other path in lua's package.path or package.cpath, respectively.
```lua
local module = require('module')
```
Be happy!

## Quirks
Since erlang and lua datatypes do not align too nicely, there are some things to consider.

- Lua has only one collection type, the table. It is lika a map in erlang. So when maps get translated to lua they become tables. 
- When lists or tuples get translated they become tables with an attached metatype 'list' or 'tuple', respectively.
- Strings in erlang are lists and translated as such. Lua has no dedicated binary type. If you want to translate to strings, use binary strings.
- Erlang has no boolean type and atoms serve no purpose in lua context. So the atom true gets translated to true and every other atom to false.

#### Translations
| Erlang | Lua | Notes |
| --- | --- | --- |
| 23 | 23 | |
| "abc" | {97, 98, 99} | erlang strings are lists |
| <<"abc">> | 'abc' | |
| [1, 2] | {1, 2} | has metatype 'list' |
| {3, 4} | {3, 4} | has metatype 'tuple' |
| #{5 => 6} | {[5] = 6} | has no metatype |
| true | true |  |
| false | false | |
| undefined | nil | in fact, every atom but true or false becomes nil |

#### Helpers
| Function | Description |
| --- | --- |
| luaport.aslist(t) | set metatype 'list' |
| luaport.astuple(t) | set metatype 'tuple' |
| luaport.asmap(t) | unset metatype |
| luaport.islist(v) | if metatype 'list' |
| luaport.istuple(v) | if metatype 'tuple' |
| luaport.ismap(v) | if no metatype |

## Help
It is very much appreciated. What i could use help with:

- Just trying out LuaPort and seeing if i made sensible choices
- Finding and fixing bugs
- Writing a more versatile makefile
- Maybe writing better english
- Things i don't know i need help with

Thank you!
