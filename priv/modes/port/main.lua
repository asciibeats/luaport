function call(...)
	return luaport.call(...)
end

function cast(...)
	return luaport.cast(...)
end

function info(...)
	luaport.info(...)
end

function callprint(...)
	print(...)
end

function calltostring(o)
	return tostring(o)
end

function getstate()
	return state
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
