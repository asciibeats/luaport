function init(...)
  print(...)
end

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
