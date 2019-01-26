function as_map(t)
	port.asmap(t)
	return t
end

function as_tuple(t)
	port.astuple(t)
	return t
end

function as_list(t)
	port.aslist(t)
	return t
end

function as_tuples(t)
	port.astuples(t)
	return t
end

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
