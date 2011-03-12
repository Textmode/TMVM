
local bitfield = require "bitfield"
local device = require "device"

local _M = {_NAME="machine", number=0}

local _MT = {__index=_M}


-------------------------------------------------------------------------
-- KEY VALUES
local maxmem      = 256 -- maximum bytes in a memory segment
local maxport     = 256 -- maximum number of device ports
local maxsegments = 256 -- maximum number of memory segments

-- speeds in cycles-per-second (hertz)
local HERTZ = 1
local KILOHERTZ = 1000*HERTZ
local MEGAHERTZ = 1000*KILOHERTZ
local GIGAHERTZ = 1000*MEGAHERTZ -- who would ever need such a ridiculous figure?

-------------------------------------------------------------------------
-- SIGNALS
local SIG_NONE                 = 0x00 -- Nothing doing.
local SIG_DIV0                 = 0x01 -- Divide by zero
local SIG_ILLEGAL_INSTRUCTION  = 0x02 -- Illegal Instruction (aka, unknown opcode)
local SIG_DEVICE_NOT_READY     = 0x03 -- attempt to use a device or port that is not in a valid state.
local SIG_DOUBLE_FAULT         = 0x0f -- Double-fault (eg, fault before clearing a fault)
local SIG_TRIPLE_FAULT         = 0xff -- Triple (halting) fault.

-- signal number -> freindly name mappings
local signals = {
	[SIG_NONE]                = "Signal Normal";
	[SIG_DIV0]                = "Divide by zero";
	[SIG_ILLEGAL_INSTRUCTION] = "Illegal Instruction";
	[SIG_DEVICE_NOT_READY]    = "Device Not Ready";
	[SIG_DOUBLE_FAULT]        = "Double Fault";
	[SIG_TRIPLE_FAULT]        = "Triple Fault (halting fault)";
	}

-------------------------------------------------------------------------
-- FLAGS
local CF = 1  -- carry flag
local SF = 2  -- sign (negative) flag
local OF = 3  -- overflow/underflow flag
local ZF = 4  -- zero flag
local PF = 5  -- parity flag
--local U1 = 6  -- unused
--local U2 = 7  -- unused
--local U3 = 8  -- unused

-- nybble parity (odd) mappings
local parity = {
	[0x0] = true;	[0x1] = false;  --	0000	0001
	[0x2] = false;	[0x3] = true;   --	0010	0011
	[0x4] = false;	[0x5] = true;   --	0100	0101
	[0x6] = true;	[0x7] = false;  --	0110	0111
	[0x8] = false;	[0x9] = true;   --	1000	1001
	[0xa] = true;	[0xb] = false;  --	1010	1011
	[0xc] = true;	[0xd] = false;  --	1100	1101
	[0xe] = false;	[0xf] = true;   --	1110	1111
}

-------------------------------------------------------------------------
-- MISC
-- number to register mappings
local cr_tbl = {[0x0]='PRM', [0x1]='A',   [0x2]='B',   [0x3]='C',   [0x4]='D',
                [0x5]='ACC', [0x6]='RET', [0xc]='FLG',[0xd]='SIG', [0xe]='SEG', [0xf]='IP' }


-------------------------------------------------------------------------
-- private helper functions

-- printf, as per C
local function printf(...)
	print(string.format(...))
end

-- a variant of io.write that include printf-like formating.
local function writef(...)
	io.write(string.format(...))
end

-- non-erroring assert-like function.
local function alert(c, m)
	if not c then print(m) end
end

-- takes a formatted free-register byte,and returns a pair of register names
local function convreg(self, n)
	local mf=math.floor
	local a, b = mf(n/16), mf(n%16)
	a, b =cr_tbl[a], cr_tbl[b]
	
	if not (a and b) then
		self:signal(SIG_ILLEGAL_INSTRUCTION)
		alert(a, ("Illegal High Register in byte: %02x"):format(n))
		alert(b, ("Illegal Low Register in byte: %02x"):format(n))
	end
	return a, b
end

-- a probably flawed rounding function.
local function round(n)
	if n > 0 then return math.floor(n)
	else return math.ceil(n) end
end

-- converts a string of bytes into the standard byte-tables used elsewhere
local function stringtodata(str)
	local t = {}
	for i=1,#str do
		t[i] = str:sub(i, i):byte()
	end
	
	return t
end

-- a coarse wait/"sleep" function.
-- involves busy-waiting
local function wait(n)
	local clk = os.clock
	local start, dt = clk()
	repeat dt = clk()-start until dt >= n
	return dt
end

-- converts an seg-offset pair into a linear address.
local function adrshift(m, adr, seg)
	assert(m and adr, "adrshift requires a segment (or machine), and an address")
	seg = (type(m)=='number' and m) or (seg or m.SEG)
	return adr+(seg*256)
end

-- returns the value stored in a given segment and offset
local function adrget(m, adr, seg)
	assert(m and adr, "adrget requires a segment (or machine), and an address")
	seg = seg or m.SEG
	return m.memory[adr+(seg*256)]
end

-- stores a value at a given segment and offset.
local function adrset(m, adr, value, seg)
	assert(m and adr, "adrset requires a segment (or machine), and an address")
	seg = seg or m.SEG
	m.memory[adr+(seg*256)] = value
end

-- sets a given CPU flag bit.
local function setflag(self, flag, bool)
	self.FLG = bitfield.SET(self.FLG, flag, bool)
	return self.FLG
end

-- Gets a given CPU flag bit.
local function getflag(self, flag, bool)
	return bitfield.GET(self.FLG, flag)
end

-- based on given unormalised value, sets and resets the flags likely
-- to have been effected by the operations leading to that value.
local	function doflags(self, value)

	if value == 0 then
		setflag(self, CF, false) -- carry/barrow
		setflag(self, SF, false) -- sign (negative)
		setflag(self, OF, false) -- overflow
		setflag(self, ZF, true)  -- zero
		setflag(self, PF, true)  -- parity
	else 
		setflag(self, ZF, false) -- zero
	end
		

	if value ~= (value % 256) then
		setflag(self, OF, true) -- overflow
		setflag(self, CF, true) -- carry/barrow
	else
		setflag(self, OF, false) -- overflow
		setflag(self, CF, false) -- carry/barrow
	end

	if value < 0x00 then
		setflag(self, SF, true) -- sign (negative)
		setflag(self, CF, true) -- carry/barrow
	else
		setflag(self, SF, false) -- sign (negative)
		setflag(self, CF, true) -- carry/barrow
	end

	do  -- Parity bit assignment
		local a = math.floor(value % 16)
		local b = math.floor(value / 16)
		a = parity[a]
		b = parity[b]
		
		setflag(self, PF, (a or b) and not (a and b)) -- XOR, for those of you following along at home
	end
	
	return value
end

-- does nothing.
local function nop() end

-------------------------------------------------------------------------
-- Instruction set
--
-- the long table of opcode-function pairs that drive this malformed beast.
_M.iset = {
	-- NOP 
	--  No OPeration, do nothing, etc.
	[0x00]=function(self) 
		return 1;
	end;
	-- INC ACC
	--  Fixed register Increment
	[0x01]=function(self)
		local r = (self.ACC+1)
		
		self.ACC = doflags(self, r) % 256
		return 1
	end;
	-- MOV &R:&R 
	--  indirect free-register move
	[0x02]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		adrset(self, self[b], adrget(self, self[a]))
		return 2;
	end;
	-- MOV .A:&nn 
	--  Move A to addresss. 
	[0x03]=function(self) 
		local point = adrget(self, self.IP+1)
		adrset(self, point, self.A)
		return 2;
	end;
	-- MOV &nn:.A 
	--  Put address contents into A
	[0x04]=function(self) 
		local point = adrget(self, self.IP+1)
		self.A = adrget(self, point)
		return 2;
	end;
	-- MOV nn:.A
	--  put literal into A
	[0x05]=function(self) 
		self.A = adrget(self, self.IP+1)
		return 2;
	end;
	-- ADD R:R -> .ACC 
	--  free-register ADDition, results in ACC
	[0x06]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local r = (self[a] + self[b])
		
		self.ACC = doflags(self, r) % 256
		return 2;
	end;
	-- SWP R:R 
	--  Free-register swap
	[0x07]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self[a], self[b] = self[b], self[a]
		return 2;
	end;
	
	
	-- SWP .A:.B (or SWP .B:.A ...)
	--  fixed register AB swap
	[0x08]=function(self) 
		self.A, self.B = self.B, self.A
		return 1;
	end;
	-- ADD .A:.B -> .ACC
	--  fixed-register AB ADDition, results in ACC
	[0x09]=function(self) 
		local r = (self.A + self.B)
		
		self.ACC = doflags(self, r) % 256
		return 1;
	end;	
	-- SHW .A
	--  fixed register Show A
	[0x0a]=function(self) 
		print("A: "..self.A)
		return 1;
	end;
	-- JMP nn (or MOV  nn:.IP
	--  unconditional jump with literal address
	[0x0b]=function(self) 
		self.IP = adrget(self, self.IP+1)
		return 0;
	end;
	-- JNZ .RET, nn (or MNZ .RET, nn:.IP)
	--  fixed-register RET conditional jump with literal address
	[0x0c]=function(self)
		if self.RET ~= 0 then
			self.IP = adrget(self, self.IP+1)
			return 0
		end
		return 2;
	end;
	-- LES .A:.B -> .RET
	--  Fixed-register AB less-than, results in RET
	[0x0d]=function(self)
		self.RET = ({[true]=1,[false]=0})[self.A < self.B]
		return 1;
	end;
	-- MNZ R, R, &nn
	--  free-register conditional move to literal address
	[0x0e]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local c = adrget(self, self.IP+2)
		
		if a == 'PRM' or b == 'PRM' then
			self:signal(SIG_ILLEGAL_INSTRUCTION)
		elseif self[a] ~= 0 then
			adrset(self, c, self[b])
		end
		return 3;
	end;
	-- MOV R:R
	--  free-register move
	[0x0f]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self[b] = self[a]
		return 2;
	end;
	
	
	-- NOT R -> R
	--  free-register Bitwise NOT in:out
	[0x10]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self[b] = bitfield:new(self[a], 8):NOT()
		return 2;
	end;
	-- AND R:R -> .RET
	--  free-register bitwise AND
	[0x11]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):AND(self[b])
		return 2;
	end;
	-- OR R:R -> .RET
	--  free-register bitwise OR
	[0x12]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):OR(self[b])
		return 2;
	end;
	-- XOR R:R -> .RET
	--  free-register bitwise XOR
	[0x13]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):XOR(self[b])
		return 2;
	end;
	-- SHL R:R -> .RET
	--  free-register shift left
	[0x14]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):shift(self[b])
		return 2;
	end;
	-- SHR R:R -> .RET
	--  free-register shift right
	[0x15]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):shift(-self[b])
		return 2;
	end;
	-- SRE R:R -> .RET
	--  free-register shift right w/ sign extension
	[0x16]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):shift(-self[b], true)
		return 2;
	end;
	-- IN R1 -> R2
	--  free-register I/O port read
	[0x17]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))

		local ret, err = self:deviceRead(self[a])
		if not ret then
			self:signal(SIG_DEVICE_NOT_READY)
		else
			self[b] = ret
		end
		return 2;
	end;
	
	
	-- OUT R1, R2
	--  free-register I/O port write
	[0x18]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))

		local ret, err = self:deviceWrite(self[a], self[b])
		if not ret then
			self:signal(SIG_DEVICE_NOT_READY)
		end
		return 2;
	end;
	-- MUL R, R -> RET
	--  Free-register Multiply
	[0x19]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		if a == 'PRM' or b == 'PRM' then
			self:signal(SIG_ILLEGAL_INSTRUCTION) end
			
		local r = round(self[a] * self[b])
		
		self.RET = doflags(self, r) % 256
		return 2;
	end;
	-- SUB R1:.R2 -> .ACC
	--  free-register ADDition, results in ACC
	[0x1a]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))

		local r = (self[a] - self[b])
		
		self.ACC = doflags(self, r) % 256
		return 2;
	end;	
	-- MOD R, R -> RET
	--  Free-register Modulo
	[0x1b]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local r =round(self[a] % self[b])
		self.RET = doflags(self, r) % 256
		return 2;
	end;
	-- MOD R, R -> ACC
	--  Free-register Modulo
	[0x1c]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local r = round(self[a] % self[b])
		
		self.ACC = doflags(self, r) % 256
		return 2;
	end;
	-- DIV R, R -> ACC
	--  Free-register divide
	[0x1d]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		if self.b == 0 then self:signal(SIG_DIV0) end
		local r = round(self[a] / self[b])
		self.ACC = doflags(self, r) % 256
		return 2;
	end;
	-- MUL R, R -> ACC
	--  Free-register Multiply
	[0x1e]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		if a == 'PRM' or b == 'PRM' then
			self:signal(SIG_ILLEGAL_INSTRUCTION) end
		
		local r = round(self[a] * self[b])
		
		self.ACC = doflags(self, r) % 256
		return 2;
	end;
	-- DEC ACC
	--  Fixed register Decrement
	[0x1f]=function(self)
		local r = (self.ACC-1)
		
		self.ACC = doflags(self, r) % 256
		return 1
	end;


	-- SUB R1:R2 -> RET
	--  Free-register SUBtraction, results in RET
	[0x20]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local r = (self[a] - self[b])
		
		self.RET = doflags(self, r) % 256
		return 2;
	end;
	-- MOV nn, B
	--  Fixed-register literal MOV to B
	[0x21]=function(self) 
		self.B = adrget(self, self.IP+1)
		return 2;
	end;	
	-- MOV &nn, B
	--  Fixed-register indirect MOV to B
	[0x22]=function(self) 
		self.B = adrget(self, adrget(self, self.IP+1))
		return 2;
	end;	
	-- MOV B, &nn
	--  Fixed-register indirect MOV from B
	[0x23]=function(self) 
		adrset(self,adrget(self, self.IP+1), self.B)
		return 2;
	end;	
	-- LMOV R1(seg),R2(off),R3(destseg), R4(destoff)
	--  free-register direct Long-move
	[0x24]=function(self)
		local a, b = convreg(self, adrget(self, self.IP+1))
		local c, d = convreg(self, adrget(self, self.IP+2))
		adrset(self, self[d], adrget(self, self[b], self[a]), self[c])
		return 3;
	end;	
	-- LJMP R1(seg), R2(off)
	--  free-register unconditional jump
	[0x25]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.SEG = self[a]
		self.IP  = self[b]
		return 0;
	end;
	-- ROL R:R -> .RET
	--  free-register roll left
	[0x26]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):roll(self[b])
		return 2;
	end;
	-- ROR R:R -> .RET
	--  free-register roll right
	[0x27]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		self.RET = bitfield:new(self[a], 8):roll(-self[b])
		return 2;
	end;


	-- EQL .A:.B -> .RET 
	--  fixed-register AB equals, results in RET
	[0xa0]=function(self) 
		self.RET = ({[true]=1,[false]=0})[self.A == self.B]
		return 1;
	end;
	-- SHW R 
	--  Free-register show
	[0xa1]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		print(a..": "..self[a])
		return 2;
	end;
	-- DIV R, R -> RET
	--  Free-register divide
	[0xa2]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		if self.b == 0 then self:signal(SIG_DIV0) end
		local r = round(self[a] / self[b])
		self.RET = doflags(self, r) % 256
		return 2;
	end;
	-- GTR .A:.B -> .RET 
	--  Fixed-register AB greater-than, results in RET
	[0xa3]=function(self) 
		self.RET = ({[true]=1,[false]=0})[self.A > self.B]
		return 1;
	end;
	-- ADD R:R -> .RET
	--  free-register ADDition, results in ACC
	[0xa4]=function(self) 
		local a, b = convreg(self, adrget(self, self.IP+1))
		local r = (self[a] + self[b])
		
		self.RET = doflags(self, r) % 256
		return 2;
	end;
	-- SUB .A:.B -> .RET
	--  fixed-register AB SUBtraction, results in RET
	[0xa5]=function(self) 
		local r = (self.A - self.B)
		
		self.RET = doflags(self, r) % 256
		return 1;
	end;	
	-- ADD .A:.B -> .RET
	--  fixed-register AB ADDition, results in RET
	[0xa6]=function(self) 
		local r = (self.A + self.B)
		
		self.RET = doflags(self, r) % 256
		return 1;
	end;	
	-- SUB .A:.B -> .ACC
	--  fixed-register AB SUBtraction, results in ACC
	[0xa7]=function(self) 
		local r = (self.A - self.B)
		
		self.ACC = doflags(self, r) % 256
		return 1;
	end;	


	-- HLT 
	--  halts the machine
	[0xff]=function(self) 
		self.state = 'halt';
		return 0;
	end;
	
	}	
	
_M.speed = 1*MEGAHERTZ -- set he default speed.

-----
-- With parm it returns the current SIGnal
-- given SIG_NONE it clears the current signal
-- given any other it acts according to the signal rules as follows:
--   if the current signal is NONE, set the given signal,
--   if the given signal is NONE, set the signal to NONE (ie, clear the signal)
--   if the current signal is nither NONE, nor DOUBLE_FAULT, signal DOUBLE_FAULT and LJMP 0,0 (aka, reset)
--   if the current signal is DOUBLE_FAULT, signal a TRIPLE_FAULT, and halt the machine.
function _M:signal(sig)
	-- if we weren't given any signal to set/trigger. then its a request
	--  for the corrent signal state.
	-- oblige by returning the current signal
	if sig == nil then return self.SIG, signals[self.SIG] end 
	
	if sig == SIG_NONE then -- if sig is none, clear all signals.
		self.SIG = SIG_NONE
		return self.SIG
	elseif self.SIG == SIG_TRIPLE_FAULT then -- you can't go worse that 3fault
		return self.SIG
	end
	
	if self.SIG == SIG_DOUBLE_FAULT then
	-- signals on 2fault is a 3fault
		self.SIG = SIG_TRIPLE_FAULT
		self.state = 'halt'
	elseif self.SIG ~= SIG_NONE then
	-- signals on an unresolved signal is a 2fault, which triggers a reset
		self.SIG = SIG_DOUBLE_FAULT
		self.IP = 0
		self.SEG = 0
	else
	-- otherwise nothign special, just set the sig register and move on
		self.SIG = sig
	end
	
	-- regardless, return the end result
	return self.SIG
end

-----
-- Creates a new machine with the given name.
-- if no name is given, a default name is chosen
function _M:new(name)

	_M.number = _M.number+1 -- keep a count of the machines created
	
	-- create a nme if we weren't given one to use.
	name = name or ("%s %02x"):format(device:generatename(device.DEV_TYPE_MACHINE),_M.number)
	
	-- raw table that makes up the machine. lots of feilds to make.
	local m = {name=name, time = 0, speed=_M.speed, memory={}, devices={}, portmap={};
		IP=0,			-- Instruction Pointer
		SEG=0,		-- SEGment Pointer
		SIG=0,		-- SIGnal register
		FLG=0,      -- Flags register
		A=0,			-- Register A
		B=0,			-- Register B
		C=0,			-- Register A
		D=0,			-- Register B
		ACC=0,		-- ACCumulator
		RET=0			-- RETurn, or result
		
		}
	setmetatable(m, _MT) -- give it wondrous fantabulous machine metatable!
	
	-- make memory non-nil, and possibly set some default values.
	for i=0,(maxmem*maxsegments) do
		m.memory[i] = 0--math.random(256)-1
	end
	
	-- install the default load-out of devices
	local t, err, d
	-- null port...
	d = device:new{type=device.DEV_TYPE_NONE}
	t, err = m:deviceInstall(d)
	assert(t, err)
	
	-- default terminal...
	d = device:new{type=device.DEV_TYPE_TERMINAL}
	t, err = m:deviceInstall(d)
	assert(t, err)

	assert(device:load('store'), "could not load")
	assert(device:load('clock'), "could not load")
	
	-- Real-time clock
	d = device:new{type=device.DEV_TYPE_CLOCK}
	t, err = m:deviceInstall(d)
	assert(t, err)
	
	
	-- persistant store (eg. HDD or FDD)
	d, err = device:new{type=device.DEV_TYPE_STORE}
	assert(d, err)
	t, err = m:deviceInstall(d)
	assert(t, err)
	
	return m
end

-----
-- Loads the given chunk or bytestring into memory.
-- as one parm (data) it load the data into 0:0
-- as two parms (start, data) it loads the data into the given address
function _M:load(start, data)
	-- starting offset is optional, but we have to shuffle things around
	-- a bit if its not given.
	if data == nil then data, start = start, 0 end
	assert(type(data)=='table' or type(data)=='string', "Invalid loadable data parm")
	
	if type(data)=='string' then data=stringtodata(data) end
	
	for i=0, #data-1 do
		assert(type(data[i+1]) == 'number' and data[i+1] == data[i+1]%256,
			"data can only contain numerical data in the range 0..255")
		self.memory[i+start] = data[i+1]
	end
end

-- creates a device and 'installs' it into the machine.
-- should creation fail, it returns nil.
--
-- Returns: dev-id
function _M:deviceInstall(dev)
	assert(dev, "Must be given a Device to install!")
	local idx, _ = #self.devices + 1
	
	-- try to find a valid portmapping
	local p_map, err = dev:findports(self.portmap)
	
	-- register the ports, and additionally check that it really
	--  is a valid mapping
	if p_map then 
		for i, p in pairs(p_map) do
			if self.portmap[p] then
				return false, "port conflict" end
			self.portmap[p]=dev:registerport(p, i)
		end
	else
		return false, err or "unknown install error"
	end
	
	-- start the device and ready it for use.
	local t, err = dev:start(self, idx)
	if t then
		self.devices[idx] = dev
	else
		return false, tostring(err)
	end
	
	return dev
end

-- attempt to use port io to write val to device id
-- 
-- returns: status-code
function _M:deviceWrite(adr, val)
	if not self.portmap[adr] then
		return false, device.DEV_STATUS_FAULTED end

	return self.portmap[adr]:writeport(adr, val)
end

-- attempt to use port io to write val to device id
-- 
-- returns: status-code
function _M:deviceRead(adr)
	if not self.portmap[adr] then 
		return false, device.DEV_STATUS_FAULTED end

	return self.portmap[adr]:readport(adr)
end

-- Causes the field-circus to services the indicated device,
--  rendering it inoperable
-- 
-- returns: (nothing)
function _M:deviceBreak(id)
	local dev = self.devices[id]
	if not dev then return end
	
	if dev.type == DEV_TERMINAL or dev.type == DEV_STREAM then
		dev.stream:close()
		self.devices[id] = false
	end
end


-----
-- Executes the given number of cycles.
-- if no number if given, it executes a single cycle.
function _M:cycle(n)
	local n = n or 1
	
	local ins, adv
	for i=1,n do
		ins = adrget(self, self.IP)
		if not self.iset[ins] then
			alert(false, ("Invalid instruction at address 0x%02x : %02x"):format(self.IP, ins))
			self:signal(SIG_ILLEGAL_INSTRUCTION)
		else
			-- Note: failure to update the segment is not an oversight.
			self.IP = self.iset[ins](self) + self.IP -- order is important here.
			self.IP = self.IP % 256 -- constrain IP to one byte values.
			self.time = self.time+1 -- and keep a count of cycles
		end
		
		-- if the machine halts, then we're done. return.
		if self.state == 'halt' then
			return 'halt'
		end
	end
	
	-- return the current state, since the caller
	-- probably wants to know anyway, this saves them a line.
	return self.state
end

-----
-- causes the machine to run at its basic speed until halted
-- if given a parm that evaluates as true, it will display a status-dump
--   approximately every second.
function _M:run(stats)
	stats = stats or false
	local t, nt, dt = os.clock()
	local drift = 0
	
	while self.state ~= 'halt' do
		drift = drift + dt
		self:cycle(self.speed)
		
		if stats then self:dump() end
		
		nt = os.clock()
		dt = nt-t
		dt = 1-dt
		if     dt > 0 then wait(dt)
--		elseif dt < 0 then
		end
		
		time = ntime
	end
end

-----
-- Dumps the status of the machine in a (hopefully) readable format.
-- intended for debugging.
function _M:dump()
	printf("%s, %d ticks", self.name, self.time)
	printf("IP:%02x\tSEG:%02x\tSIG:%02x\tACC:%02x\tRET:%02x\tA:%02x\tB:%02x",
		self.IP, self.SEG, self.SIG, self.ACC, self.RET, self.A, self.B)
	print()
	
	-- one 256 byte block as 16x16 hex-pairs.
	for i=0,255, 16 do
		for j=0,15 do
		writef("%02x ", adrget(self, i+j))
		end
		print()
	end
	 
end

-- set the default call for a machine to be Cycle
_MT.__call = _M.run


-- possibly clever shit, to let us be both a valid module, and a valid program.
if arg and arg[0] and (arg[0]=='machine.lua' or arg[0]=='machine') then

	print("Begining...")
	local machnum = 1
	local machs = {}

	local tmp, tmpfile, err
	for i=1,machnum do
		tmp, err = _M:new()
		assert(tmp, "Failed to create required machine: "..tostring(err))

		tmpfile = string.format("machine-%02d.init", i)
		tmpfile, err = io.open(tmpfile, "rb")
		if tmpfile then
			tmp:load(tmpfile:read('*a'))
			tmpfile:close()
		else
			printf("couldn't open 'machine-%02d.init': %s", i, tostring(err))
		end

		machs[i] = tmp
	end

	local time, ntime = os.clock(), 0
	local dt, ndt, drift = 0, 0, 0
	
	print("^C to stop...")
	while true do
		drift = drift + dt
		for j=1, 5 do
			print('cycle')
			for i=1,#machs do
				tmp = machs[i]
				tmp:cycle(tmp.speed/10)
				print('next!')
			end
		end
		ntime = os.clock()
		dt = ntime-time
		ndt = 1-dt
		if     dt > 0 then dt = wait(ndt); ndt = dt - ndt
		elseif dt < 0 then io.stderr:write("Cycle overtime\n")
		end
		dt=ndt
		time = ntime
	end

end

-------------------------------------------------------------------------
-- MODULE TAIL

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

