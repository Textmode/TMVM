
local _M = {}
_M._NAME = "bitfield"
_M._TYPE = 'module'
local _MT = {}

local nan = 1%0
local inf = 1/0

_M.width = 16

local function isbf(a)
	return type(a)=='table' and a._TYPE=='bitfield'
end

-- I really shouldn't need this
local function bin(n)
  assert(type(n) == 'number', "dub requires a number")
  return 2^(n-1)
end


function _M:new(n, opt_width)
	n = (type(n)=='number' and n) or 0
	local b = {value = n, _TYPE = 'bitfield', width = opt_width or _M.width}
	setmetatable(b, _MT)
	return b
end

function _MT.GET(self, n)
	if type(n) ~= 'number' then return _M[n] end
	if n < 0 then return nil end

	return self.value % (2*bin(n)) >= bin(n)
end

_MT.__index = _M.GET --(self, idx, val)

function _M.SET(self, n, v)
	assert(n > 0, "Cannot set imaginary bits.")
	n = bin(n)

	-- should I simply use the normal lua rules of truth?
	if type(v) == 'number' then
		v = ({[true]=1, [false]=0})[v~=0] -- false = 0, true = !false
	end
	
	
	if v then
		if self.value % (2*n) < n then
			self.value = self.value + n
		end
	else -- false/clear
		if self.value % (2*n) >= n then
			self.value = self.value - n
		end
	end
end

_MT.__newindex = _M.SET --(self, idx, val)

function _MT.__tostring(self)
	local s ={}
	for i=self.width,1,-1 do s[#s+1]=({[true]='1', [false]='0'})[b[i]] end
	s[#s+1]="b"
	return table.concat(s)
end

--(based on code from [ http://lua-users.org/wiki/BitUtils ]
function _M:XOR(n, y)
	local x,width = n, 32
	if isbf(n) then x,width = n.value, n.width end
	
	local z = 0
	for i = 0, width-1 do
		if (x % 2 == 0) then                      -- x had a '0' in bit i
			if ( y % 2 == 1) then                  -- y had a '1' in bit i
				y = y - 1 
				z = z + 2 ^ i                       -- set bit i of z to '1' 
			end
		else                                      -- x had a '1' in bit i
			x = x - 1
			if (y % 2 == 0) then                   -- y had a '0' in bit i
				z = z + 2 ^ i                       -- set bit i of z to '1' 
			else
				y = y - 1 
			end
		end
		y = y / 2
		x = x / 2
	end
	
	if isbf(n) then n.value = z end
	return z
end

function _M.NOT(n)
	n = (n and isbf(n)) or _M:new(n)
	local r = (2^n.width - 1) - n.value
	a.value = r
	return r
end

function _M.OR(a,b)
	a = a and isbf(a) or _M:new(a)
	b = b and isbf(b) or _M:new(b)
	local max = (2^a.width - 1)
	local r = max - _M.AND(max - a, max - b)
	a.value = r
	return r
end

function _M.AND(a,b)
	a = a and isbf(a) or _M:new(a)
	b = b and isbf(b) or _M:new(b)
	local r = ((a+b) - _M:XOR(a, b))/2
	a.value = r
	return r
end

function _M.NAND(a,b)
	a = a and isbf(a) or _M:new(a)
	b = b and isbf(b) or _M:new(b)
	local r = _M.NOT(_M.AND(a, b))
	a.value = r
	return r
end

function _M.NOR(a,b)
	a = a and isbf(a) or _M:new(a)
	b = b and isbf(b) or _M:new(b)
	local r = _M.NOT(_M.OR(a, b))
	a.value = r
	return r
end

function _M.NXOR(a,b)
	a = a and isbf(a) or _M:new(a)
	b = b and isbf(b) or _M:new(b)
	local r = _M.NOT(_M.XOR(a, b))
	a.value = r
	return r
end


-------------------------------------------------------------------------
-- MODULE TAIL

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

