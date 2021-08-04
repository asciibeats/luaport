local port = require('port')
local some, values = port.call.init('sunshine')
local a, b = unpack(config or {})

print('hello', some, values)
print('config', a, b)

function echo(...)
  print(...)
  return ...
end

function call(name, ...)
  return port.call[name](...)
end

function cast(name, ...)
  port.cast[name](...)
end

function exec(name, ...)
  return port[name](...)
end

function after(time, ...)
  return port.after(time, function (s, ...) print(s, ...) end, 'after', ...)
end

function interval(time, ...)
  local i = 0
  return port.interval(time, function (s, ...) print(s, i, ...); i = i + 1 end, 'interval', ...)
end

function sleep(time)
  port.sleep(time)
end
