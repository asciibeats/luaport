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
  luaport.after(time, function (s) print(s, time, note) end, 'after')
end

function interval(time)
  local i = 0
  luaport.interval(time, function (s) print(s, time, i); i = i + 1 end, 'interval')
end
