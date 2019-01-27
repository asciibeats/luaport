function call(...)
	return port.call(...)
end

function cast(...)
	return port.cast(...)
end

function info(...)
	port.info(...)
end

function callprint(...)
	print(...)
end

function getstate()
	return state
end

function asmap(t)
	return port.asmap(t)
end

function astuple(t)
	return port.astuple(t)
end

function aslist(t)
	return port.aslist(t)
end

function astuplelist(t)
	return port.astuplelist(t)
end
