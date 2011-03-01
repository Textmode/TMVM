
local _M = {}
_M._NAME = "bitfield"
_M._TYPE = 'module'
local _MT = {}

local nan = 1%0
local inf = 1/0

-- I really shouldn't need this
local function bin(n)
  assert(n, "dub requires a parm")
  assert(type(n) == 'number', "dub requires a number")
  return 2^(n-1)
end


function _M:new(n)
	n = n or 0
	local b = {value = n, _TYPE = 'bitfield'}
	setmetatable(b, _MT)
	return b
end

function _MT.__index(self, n, ...)
	if type(n) ~= 'number' then return _M[n] end
	if n < 0 then return nil end

	return self.value % (2*bin(n)) >= bin(n)
end

function _MT.__newindex(self, n, v)
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

function _MT.__tostring(self)
	-- TODO: support arbitary bit-widths
	local s ={}
	for i=8,1,-1 do s[#s+1]=({[true]='1', [false]='0'})[b[i]] end
	s[#s+1]="b"
	return table.concat(s)
end

--(based on code from [ http://lua-users.org/wiki/BitUtils ]
function _M:xor(n, y)
	local x = n
	if type(n) == 'table' and n._TYPE and n._TYPE == 'bitfield' then x = n.value end
	
	local z = 0
	for i = 0, 31 do
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
	
	if type(n) == 'table' and n._TYPE and n._TYPE == 'bitfield' then n.value = z end
	return z
end

-------------------------------------------------------------------------
-- MODULE TAIL

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

