function init(options)
  luaport.cast.print('hallo')
  local result = luaport.call.multiply(2, 21)
  print('init', options, result)
end

function echo(...)
  print(...)
  return ...
end

function call(name, ...)
  print('call', name, ...)
  luaport.call[name](...)
  return luaport.call[name](...)
end

function cast(name, ...)
  print('cast', name, ...)
  luaport.cast[name](...)
  luaport.cast[name](...)
end

function getnil()
  return nil
end

function execute(name, ...)
  return luaport[name](...)
end
