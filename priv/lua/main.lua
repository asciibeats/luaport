local some, values = port.call.init('sunshine')

print('hello', some, values)

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
