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

function getstate()
  return state, nil
end

function execute(name, ...)
  return luaport[name](...)
end
