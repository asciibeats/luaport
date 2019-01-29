function call(name, ...)
	return luaport.call[name](...)
end

function cast(name, ...)
	luaport.cast[name](...)
end

function info(...)
	print(...)
end

function calltostring(v)
	return tostring(v)
end

function calltype(v)
	return type(v)
end

function getstate()
	return state
end

function getnil()
	return nil
end

function ismap(t)
	return luaport.ismap(t)
end

function islist(t)
	return luaport.islist(t)
end

function istuple(t)
	return luaport.istuple(t)
end

function isatom(a)
	return luaport.isatom(a)
end

function asmap(t)
	return luaport.asmap(t)
end

function astuple(t)
	return luaport.astuple(t)
end

function aslist(t)
	return luaport.aslist(t)
end

function astuplelist(t)
	return luaport.astuplelist(t)
end

function toatom(s)
	return luaport.toatom(s)
end
