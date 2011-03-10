
local _M = {}
_M._NAME = "bitfield"
_M._TYPE = 'module'
local _MT = {}

local nan = 1%0
local inf = 1/0

_M.width = 16

-- checks if a is a bitfield table, or not.
local function isbf(a)
	return (type(a)=='table' and a._TYPE=='bitfield') and true or false
end

-- I really shouldn't need this
local function bin(n)
  assert(type(n) == 'number', "dub requires a number")
  return 2^(n-1)
end

-- Creates and returns a new bitfield. If opt_num is provided, the initial
--  value of the bitfield will be the binary representation of that number,
--  otherwise it will be zero. If opt_width is provided t will be used as 
--  the effective bitwidth of the bitfield, otherwise it will default to 
--  16 bits.
--
-- Returns: bitfield
function _M:new(n, opt_width)
	n = (type(n)=='number' and n) or 0
	local b = {value = n, _TYPE = 'bitfield', width = opt_width or _M.width}
	setmetatable(b, _MT)
	return b
end

-- returns true if the nth bit is set, otherwise returns false. if a bit
--  outside the range of the bitfield's width is requested, it returns nil
--  instead (which is also logically false)
-- 
-- Returns: bit
function _M.GET(self, n)
	if type(n) ~= 'number' then return _M[n] end
	if n < 0 then return nil end

	return self.value % (2*bin(n)) >= bin(n)
end

_MT.__index = _M.GET --(self, idx)

-- Sets the nth bit of bf to the truth value of v
-- 
-- Returns: (nothing)
function _M.SET(b, n, v)
	if not isbf(b) then b = bitfield(b) end
	assert(n > 0, "Cannot set imaginary bits.")
	n = bin(n)

	-- should I simply use the normal lua rules of truth?
	if type(v) == 'number' then
		v = ({[true]=1, [false]=0})[v~=0] -- false = 0, true = !false
	end
	
	
	if v then
		if b.value % (2*n) < n then
			b.value = self.value + n
		end
	else -- false/clear
		if b.value % (2*n) >= n then
			b.value = self.value - n
		end
	end
	
	return b.value
end

_MT.__newindex = _M.SET --(self, idx, val)

-- Returns a string representing the value contained in the bitfield in
--  binary.
--
-- Returns: string
function _MT.__tostring(self)
	local s ={}
	for i=self.width,1,-1 do s[#s+1]=({[true]='1', [false]='0'})[self[i]] end
	s[#s+1]="b"
	return table.concat(s)
end

-- XORs the value of bf with b. Additionally returns the resulting value.
--
--    0 XOR 0 -> 0
--    0 XOR 1 -> 1
--    1 XOR 0 -> 1
--    1 XOR 1 -> 0
-- 
-- returns: num
function _M.XOR(a, b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	
	for i = 1, a.width do 
		a[i] = a[i] ~= b[i]
	end
	
	a.value = a.value % bin(a.width+1)
	return a.value
end

-- Negates the value of a. Additionally returns the resulting value
--
--    NOT 1 -> 0
--    NOT 0 -> 1
--
-- returns: num
function _M.NOT(n)
	n = (isbf(n) and n) or _M:new(n)
	local r = (2^n.width - 1) - n.value
	n.value = r % bin(n.width+1)
	return n.value
end

-- ORs the value of a with b. Additionally returns the resulting value.
--
--    0 OR 0 -> 0
--    0 OR 1 -> 1
--    1 OR 0 -> 1
--    1 OR 1 -> 1
--
-- Returns: num
function _M.OR(a,b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	local max = (2^a.width - 1)
	local r = max - _M.AND(max - a.value, max - b.value)
	a.value = r % bin(a.width+1)
	return a.value
end

-- ANDs the value of a with b. Additionally returns the resulting value.
--
--    0 AND 0 -> 0
--    0 AND 1 -> 0
--    1 AND 0 -> 0
--    1 AND 1 -> 1
--
-- Returns: num
function _M.AND(a,b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	local r = ((a.value+b.value) - _M.XOR(a, b))/2
	a.value = r % bin(a.width+1)
	return a.value
end

-- NANDs the value of a with b. Additionally returns the resulting value.
--
--    0 NAND 0 -> 1
--    0 NAND 1 -> 1
--    1 NAND 0 -> 1
--    1 NAND 1 -> 0
--
-- Returns: num
function _M.NAND(a,b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	local r = _M.NOT(_M.AND(a, b))
	a.value = r % bin(a.width+1)
	return a.value
end

-- NORs the value of a with b. Additionally returns the resulting value.
--
--    0 NOR 0 -> 1
--    0 NOR 1 -> 0
--    1 NOR 0 -> 0
--    1 NOR 1 -> 0
--
-- Returns: num
function _M.NOR(a,b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	local r = _M.NOT(_M.OR(a, b))
	a.value = r % bin(a.width+1)
	return a.value
end

-- XNORs the value of a with b. Additionally returns the resulting value.
-- 
--    0 XNOR 0 -> 1
--    0 XNOR 1 -> 0
--    1 XNOR 0 -> 0
--    1 XNOR 1 -> 1
--
-- Return: num
function _M.XNOR(a,b)
	a = (isbf(a) and a) or _M:new(a)
	b = (isbf(b) and b) or _M:new(b)
	local r = _M.NOT(_M.XOR(a, b))
	a.value = r % bin(a.width+1)
	return a.value
end

-- Shifts the value of a by n. Additionally returns the resulting value.
--  Positive values shift left, negative values shift right. if sign-ext
--  is true, then the sign bit will be extended during a right-shift.
--
-- Returns: num
function _M.shift(a, n, sinex)
	a = (isbf(a) and a) or _M:new(a)
	if n == 0 then return a.value end

	local r = a.value
	if n > 0 then
		for i=1,n do
			r = r+r
		end
	else -- less than 0, shift right
		local flr = math.floor
		local s = a[a.width]

		for i=1,math.abs(n) do
			r = flr(r/2)
		end
		
		if sinex then
			a.value = r
			a[a.width] = s
			r = a.value
		end
	end
	a.value = math.floor(r % bin(a.width+1))
	return a.value
end

-- Rolls the value of a by n. Additionally returns the resulting value.
--  Positive values roll left, negative values roll right.
-- A roll is similar to a shift, however values that fall of one end
--  simply return to the other end. thus rolling 100b left one would be
--  001b. and again would be 010b
--   (thanks to GeDaMo)
-- Returns: num
function _M.roll(a, n)
	a = (isbf(a) and a) or _M:new(a)
	
	if n == 0 then return a.value end

	if (a.value >= 2^a.width-1) then
		--print(a.value, a.value)
	else
		a.value = (a.value * 2^(n % a.width)) % (2^a.width-1)
	end
	
--	a.value = math.floor(r % bin(a.width)+1)
	return a.value
end


-------------------------------------------------------------------------
-- MODULE TAIL

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

