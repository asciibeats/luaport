function fail()
	a[1] = 0
end

function callerror(message, level)
	error(message, level)
end

function echo(...)
	return ...
end
