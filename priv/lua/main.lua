local port = require('port')
local some, values = port.call.init('sunshine')

print('hello', some, values)

function Echo(...)
  print(...)
  return ...
end

function Call(name, ...)
  return port.call[name](...)
end

function Cast(name, ...)
  port.cast[name](...)
end

function Exec(name, ...)
  return port[name](...)
end

function After(time, ...)
  return port.after(time, function (s, ...) print(s, ...) end, 'after', ...)
end

function Interval(time, ...)
  local i = 0
  return port.interval(time, function (s, ...) print(s, i, ...); i = i + 1 end, 'interval', ...)
end

function Sleep(time)
  port.sleep(time)
end