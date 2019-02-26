print('hello')

local some, values = luaport.call.init('sunshine')

function echo(...)
  return ...
end

function call(name, ...)
  return luaport.call[name](...)
end

function cast(name, ...)
  luaport.cast[name](...)
end

function exec(name, ...)
  return luaport[name](...)
end

function after(time, note)
  local ref = luaport.after(time, function (s) print(s, time, note) end, 'after')
  luaport.cancel(ref)
end

function interval(time)
  local i = 0
  local ref = luaport.interval(time, function (s) print(s, time, i); i = i + 1 end, 'interval')
  luaport.cancel(ref)
end
