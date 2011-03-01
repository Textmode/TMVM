
local bitfield = require "bitfield"

local _M = {_NAME="machine", number=0}

local _MT = {__index=_M}


-------------------------------------------------------------------------
-- KEY VALUES
local maxmem = 256

local HERTZ = 1
local KILOHERTZ = 1000*HERTZ
local MEGAHERTZ = 1000*KILOHERTZ

-------------------------------------------------------------------------
-- SIGNALS
local SIG_NONE                 = 0x00 -- Nothing doing.
local SIG_DIV0                 = 0x01 -- Divide by zero
local SIG_ILLEGAL_INSTRUCTION  = 0x02 -- Illegal Instruction (aka, unknown opcode)
local SIG_DOUBLE_FAULT         = 0x0f -- Double-fault (eg, fault before clearing a fault)
local SIG_TRIPLE_FAULT         = 0xff -- Triple (halting) fault.

signals = {
	[SIG_NONE]          = "Signal Normal";
	[SIG_DIV0]          = "Divide by zero";
	[SIG_DOUBLE_FAULT]  = "Double Fault";
	[SIG_TRIPLE_FAULT]  = "Triple Fault (halting fault)";
	}

-------------------------------------------------------------------------
-- MISC
local cr_tbl = {[0x0]='PRM', [0x1]='A',   [0x2]='B', [0x3]='ACC', [0x4]='RET',
                [0xd]='SIG', [0xe]='SEG', [0xf]='IP' }


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
	if x > 0 then return math.floor(n)
	else return math.ciel(n) end
end

-- converts a string of bytes into the standard bytetables used elsewhere
local function stringtodata(str)
	local t = {}
	for i=1,#str do
		t[i] = str:sub(i, i):byte()
	end
	
	return t
end

-- a very coarse and generally poor wait/"sleep" function.
local function wait(n)
	local t, nt, dt = os.time(), nil, 0
	repeat
		nt = os.time()
		dt = os.difftime(t, nt)
	until dt >= n
	return dt
end

-------------------------------------------------------------------------
-- Instruction set
--
-- the long table of opcode-function pairs that drive this malformed beast.
_M.iset = {
	[0x00]=function(self) --  NOP # No OPeration, do nothing, etc.
		return 1;
	end;
	[0x02]=function(self) --  MOV &R:&R # indirect free-register move
		local a, b = convreg(self, self.memory[self.IP+1])
		self.memory[self[b]] = self.memory[self[a]]
		return 2;
	end;
	[0x03]=function(self) --  MOV .A:&nn # Move A to addresss. 
		local point = self.memory[self.IP+1]
		self.memory[point] = self.A
		return 2;
	end;
	[0x04]=function(self) --  MOV &nn:.A # Put address contents into A
		local point = self.memory[self.IP+1]
		self.A = self.memory[point]
		return 2;
	end;
	[0x05]=function(self) --  MOV nn:.A	# put literal into A
		local point = self.memory[self.IP+1]
		self.A = point
		return 2;
	end;
	[0x06]=function(self) --  ADD R:R -> ACC # free-register ADD, results in ACC
		local a, b = convreg(self, self.memory[self.IP+1])
		self.ACC = self[a] + self[b] % 256
		return 2;
	end;	
	[0x07]=function(self) --  SWP R:R # Free-register swap
		local a, b = convreg(self, self.memory[self.IP+1])
		self[a], self[b] = self[b], self[a]
		return 2;
	end;	
	[0x08]=function(self) --  SWP .A:.B (or SWP .B:.A ...) # fixed register AB swap
		self.A, self.B = self.B, self.A
		return 1;
	end;	
	[0x09]=function(self) --  ADD .A:.B -> ACC # fixed-register AB ADD, results in ACC
		self.ACC = self.A + self.B % 256
		return 1;
	end;	
	[0x0a]=function(self) --  SHW .A # fixed register Show A
		print("A: "..self.A)
		return 1;
	end;
	[0x0b]=function(self) --  JMP nn (or if you prefer, MOV nn:.IP) unconditional jump with literal address
		self.IP = self.memory[self.IP+1]
		return 0;
	end;
	[0x0c]=function(self) --  JNZ RET, nn (or MNZ RET, nn:.IP) # fixed-register RET conditional jump with literal address
		if self.RET ~= 0 then
			self.IP = self.memory[self.IP+1]
			return 0
		end
		return 2;
	end;
	[0x0d]=function(self) --  LES .A:.B -> RET # Fixed-register AB less-than, results in RET
		self.RET = ({[true]=1,[false]=0})[self.A < self.B]
		return 1;
	end;
	[0x0e]=function(self) --  MNZ R, R, &nn # free-register conditional move to literal address
		local a, b = convreg(self, self.memory[self.IP+1])
		local c = self.memory[self.IP+2]
		
		if a == 'PRM' or b == 'PRM' then
			self:signal(SIG_ILLEGAL_INSTRUCTION)
		elseif self[a] ~= 0 then
			self.memory[c] = self[b]
		end
		return 3;
	end;
	[0x0f]=function(self) --  MOV R:R # free-register move
		local a, b = convreg(self, self.memory[self.IP+1])
		self[b] = self[a]
		return 2;
	end;
	[0x10]=function(self) --  NOT R -> R # free-register NOT in:out
		local a, b = convreg(self, self.memory[self.IP+1])
		self[b] = bitfield:new(self[a], 8):NOT()
		return 2;
	end;
	[0x11]=function(self) --  AND R:R ->RET # free-register AND
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):AND(self[b])
		return 2;
	end;
	[0x12]=function(self) --  OR R:R ->RET # free-register OR
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):OR(self[b])
		return 2;
	end;
	[0x13]=function(self) --  XOR R:R ->RET # free-register XOR
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):XOR(self[b])
		return 2;
	end;
	[0x14]=function(self) --  SHL R:R ->RET # free-register shift left
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):shift(self[b])
		return 2;
	end;
	[0x15]=function(self) --  SHR R:R ->RET # free-register shift right
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):shift(-self[b])
		return 2;
	end;
	[0x16]=function(self) --  SRE R:R ->RET # free-register shift right w/ sign extension
		local a, b = convreg(self, self.memory[self.IP+1])
		self.RET = bitfield:new(self[a], 8):shift(-self[b], true)
		return 2;
	end;
	[0xa0]=function(self) --  EQL .A:.B -> RET # fixed-register AB equals, results in RET
		self.RET = ({[true]=1,[false]=0})[self.A == self.B]
		return 1;
	end;
	[0xa1]=function(self) --  SHW R # Free-register show
		local a, b = convreg(self, self.memory[self.IP+1])
		print(a..": "..self[a])
		return 2;
	end;
	[0xa2]=function(self) --  DIV R, R # Free-register divide
		local a, b = convreg(self, self.memory[self.IP+1])
		if self.b == 0 then self:signal(SIG_DIV0) end
		self.RET = round((self[a] / self[b]) % 256)
		return 2;
	end;
	[0xa3]=function(self) --  GTR .A:.B -> RET # Fixed-register AB greater-than, results in RET
		self.RET = ({[true]=1,[false]=0})[self.A > self.B]
		return 1;
	end;

	[0xff]=function(self) --  HLT # halts the machine
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
	if sig == nil then return self.SIG, signals[self.SIG] end
	
	if sig == SIG_NONE then
		self.SIG = SIG_NONE
	elseif self.SIG == SIG_DOUBLE then
		self.SIG = SIG_TRIPLE_FAULT
		self.state = 'halt'
	elseif self.SIG ~= SIG_NONE then
		self.SIG = SIG_DOUBLE_FAULT
		self.IP = 0
		self.SEG = 0
	else	
		self.SIG = sig
	end
	
	return self.SIG
end

-----
-- Creates a new machine with the given name.
-- if no name is given, a default name is chosen
function _M:new(name)

	_M.number = _M.number+1
	
	local m = {name=name or ("Machine_%02x"):format(_M.number), time = 0,speed=_M.speed, memory={};
		IP=0,			-- Instruction Pointer
		SIG=0,		-- SIGnal register
		SEG=0,		-- SEGment Pointer
		A=0,			-- Register A
		B=0,			-- Register B
		ACC=0,		-- ACCumulator
		RET=0			-- RETurn, or result
		}
	setmetatable(m, _MT)
	
	for i=0,maxmem do
		m.memory[i] = 0--math.random(256)-1
	end
	
	return m
end

-----
-- Loads the given chunk or bytestring into memory.
-- as one parm (data) it load the data into 0:0
-- as two parms (start, data) it loads the data into the given address
function _M:load(start, data)
	if data == nil then data, start = start, 0 end
	assert(type(data)=='table' or type(data)=='string', "Invalid loadable data parm")
	
	if type(data)=='string' then data=stringtodata(data) end
	
	for i=0, #data-1 do
		assert(type(data[i+1]) == 'number' and data[i+1] == data[i+1]%256,
			"data can only contain numerical data in the range 0..255")
		self.memory[i+start] = data[i+1]
	end
end

-----
-- Executes the given number of cycles.
-- if no number if given, it executes a single cycle.
function _M:cycle(n)
	local n = n or 1
	
	local ins, adv
	for i=1,n do
		ins = self.memory[self.IP]
		if not self.iset[ins] then
			alert(false, ("Invalid instruction at address 0x%02x : %02x"):format(self.IP, ins))
			self:signal(SIG_INVALID_INSTRUCTION)
		end

		self.IP = self.iset[ins](self) + self.IP -- order is important here.
		self.IP = self.IP % 256
		self.time = self.time+1	
		
		if self.state == 'halt' then return 'halt' end
	end
	return self.state
end

-----
-- BROKEN!
-- causes the machine to run at its basic speed until halted
-- if given a parm that evaluates as true, it will display a status-dump
--   approximately every second.
function _M:run(stats)
	stats = stats or false
	local time, ntime, dt = os.time(), nil, nil
	
	while self.state ~= 'halt' do
		self:cycle(self.speed)
		
		if stats then self:dump() end
		
		ntime = os.time()
		dt = os.difftime(time, ntime)
		if     dt < 1 then wait(1)
		elseif dt > 1 then print("woops, too slow!")
		end
		
		print(self.time)
		time = ntime
	end
end

-----
-- Dumps the status of the machine in a (hopefully) readable format.
-- intended for debugging.
function _M:dump()
	printf("%s, %d ticks", self.name, self.time)
	printf("IP:%02x\tACC:%02x\tRET:%02x\tA:%02x\tB:%02x", self.IP, self.ACC, self.RET, self.A, self.B)
	print()
	
	local ram = self.memory
	for i=0,255, 16 do
		for j=0,15 do
		writef("%02x ", ram[i+j])
		end
		print()
	end
	 
end

-- set the default call for a machine to be Cycle
_MT.__call = _M.run


-- possibly clever shit
if arg and arg[0] then
	print("Begining...")
	local machnum = 1
	local machs = {}

	local inf, outf, err = arg[1], arg[2], ""
	
	local tmp
	for i=1,machnum do
		tmp = _M:new()
		assert(tmp, "Failed to create required machine")
		machs[i] = tmp
	end

	local time, ntime, dt= os.time(), nil, nil
	print("^C to stop...")
	while true do
		for i=1,#machs do
			tmp = machs[i]
			tmp:cycle(tmp.speed)
		end
		ntime = os.time()
		dt = os.difftime(time, ntime)
		if     dt < 1 then wait(1)
		elseif dt > 1 then print("woops, too slow!")
		end
		time = ntime
	end

end

-------------------------------------------------------------------------
-- MODULE TAIL

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

