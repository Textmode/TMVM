
-- 'desire' functions similarly to 'require', save that failure is not an error.
-- use this if you can make use of a modules features, but can make do without.
--
-- if successful it returns the same value that require would return
-- (either true, or the actual module table) if unsuccessful, it returns
-- false, and a string indicating the error.
function desire(name, ...)
	local p, m, err = pcall(require, n, ...)
	if p then
		return m
	else -- p == false
		return p, m
	end
end

-- inquire determines if a package is loaded.
-- if so, it returns the value that 'require' would return.
-- otherwise, it returns false and the string "not loaded"
function inquire(name, ...)
	if package.loaded[name] then
		return package.loaded[name]
	else
		return false, "not loaded"
	end
end
