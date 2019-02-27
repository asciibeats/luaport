local some, values = luaport.call.init('sunshine')

print('hello', some, values)

function echo(...)
  print(...)
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

function after(time, ...)
  return luaport.after(time, function (s, ...) print(s, ...) end, 'after', ...)
end

function interval(time, ...)
  local i = 0
  return luaport.interval(time, function (s, ...) print(s, i, ...); i = i + 1 end, 'interval', ...)
end

function cancel(ref)
  luaport.cancel(ref)
end
