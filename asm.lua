local _M = {_NAME="asm", number=0}

local _MT = {__index=_M}

local symbols, len -- needed for parsing

local regnum = {PRM=0x0, A  =0x1, B=0x2, C=0x3, D=0x4, ACC=0x5, RET=0x6,
                FLG=0xc, SIG=0xd, SEG=0xe, IP =0xf}
local regs = {'A', 'B', 'C', 'D', 'ACC', 'RET', 'FLG', 'SIG', 'SEG', 'IP'} 

-- takes two strings indicating registers, and encodes them into a
-- single free-register byte (taken by some opcodes)
local function reg_encode(a, b)
	a = a or 'PRM'
	b = b or 'PRM'

	assert(regnum[a] and regnum[b], "Invalid Register passed to reg_encode")
	return (regnum[a]*16) + regnum[b]
end

-- trys to figure out the base of a number, and process it accordingly.
local function parsenum(n)
	if type(n) == 'number' then return n end -- our work here is done!
	assert(type(n) == 'string', "Cannot parse non-text")

	if n:sub(1,1) == "$" then  -- hexadecimal
		return tonumber(n:sub(2), 16)
	elseif n:sub(-1,-1) == 'h' then -- hexadecimal again
		return tonumber(n:sub(1, -2), 16)
	elseif n:sub(1,1) == "%" then -- binary
		return tonumber(n:sub(2), 2)
	elseif n:sub(1,1) == "0" then  -- Octal
		return tonumber(n, 8)
	elseif n:match("'(%\?.)'") then  -- char-literal
		return string.byte(n:match("'(.)'"))
	else  -- doesn't seem to be anything special, decimal?
		return tonumber(n, 10) 
	end
end

-- takes a string indicating a potential parameter, and tries to figure
-- out its type value, and directness.
--
-- returns: value_type, pDirect, value
local function parm(p)
	assert(p, "Can't check nil parms!")
	p = string.match(p, "%S+")
	local form, abs, value = 'unk', not (p:sub(1, 1) == '&'), p:match("\&?(.*)")
	p = value
	
	if p == 'null' then return 'symbol', abs, 'null' end -- null is always availible, and never set
	
	for i=1,#regs do
		form = regs[i] == p and 'register' or form
	end
	
	if form ~= 'register' then 
		value = parsenum(p)
		form = 'literal'
		
		if value == nil then
			value = symbols[p]
			if value then 
				return form, abs, value
			else
				form = 'symbol'
				value = p
			end
				
			
		end
	end
	
	return form, abs, value
end

-- the encoder functions take a list of raw parameters and parse them
-- according to the expectations of the instruction they represent.
-- having done this, they then attempt to find an opcode that accepts that
-- combination of parms ( generally starting with the best/most specific
-- opcodes, and ending with the most generic) and finally attempt to encode
-- a string representing that combination of opcode(s) and parameters.
--
-- certain encoders represent psudo or meta instructions, which do things
-- like define symbols (LET, LBL) or include data directly in the stream
-- (BYTE, DATA)
--
-- regardless, the encoder ends by returning either the encoded chunk, or 
-- false, followed by a message explaining the error.
local encoders = {
	NOP = function(a, b, c)
		-- NOP is the do-nothing instruction. it has only one form.
		if not (a or b or c) then
			return false, "NOP must be properly qualified: 'NOP'" end
		return string.char(0x00)	
	end;
	MOV = function(a, b, c)
		-- MOV is the move data instruction.
		-- MOV easily has the most forms of any instruction in TMVM/FUASSM.
		-- and as a result this is easily the most complicated and 
		-- error-prone encoder in FUASSM.
		if not (a and b and not c) then
			return false, "MOV must be properly qualified: 'MOV P1,P2'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if ((af=='literal' or af=='symbol') and (bf=='literal' or bf=='symbol')) then
			return false, "All move operations must involve a register, sorry." end
		
		if af == 'register' and bf == 'register' then -- free-register form
			if aa == true and ba == true then -- pure register form
				return string.char(0x0f, reg_encode(av, bv))
			elseif aa == false and ba == false then --pure addressed register form
				return string.char(0x02, reg_encode(av, bv))
			end
		end
		
		-- resolved symbols are litteral numbers, thus the literal and symbolic
		-- forms are common to eachother, differing mostly in if they still require 
		-- 'patching', or not.
		if (af=='literal' or af=='symbol') and bf=='register' then
			if aa == true and ba == true then
				if bv == "A" then
					return string.char(0x05, (af=='literal' and tonumber(av)) or 0), af=='symbol'
				elseif bv == "B" then
					return string.char(0x21, (af=='literal' and tonumber(av)) or 0), af=='symbol'
				else
					return false, "No encodings for mixed-mode moves not involving registers A or B"
				end
			elseif aa == false and ba == true then -- mixed get form
				if bv == "A" then
					return string.char(0x04, (af=='literal' and tonumber(av)) or 0), af=='symbol'
				elseif bv == "B" then
					return string.char(0x22, (af=='literal' and tonumber(av)) or 0), af=='symbol'
				else
					return false, "No encodings for mixed-mode moves not involving registers A or B"
				end
			end
		end
		
		-- as above.
		if af == 'register' and (bf=='literal' or bf=='symbol') then
			if aa == true and ba == false then -- mixed push/set form
				if av == "A" then
					return string.char(0x03, (bf=='literal' and tonumber(bv)) or 0), bf=='symbol'
				elseif av == "B" then
					return string.char(0x23, (bf=='literal' and tonumber(bv)) or 0), bf=='symbol'
				else
					return false, "No encodings for mixed-mode moves not involving registers A or B"
				end
			else
				return false, "Impossible operation, perhaps you meant P2 to be indirect?"
			end
		end
	end;
	LMOV = function(a, b, c, d)
		-- the long-move instrution, used to move data too, from, or between
		-- different segments. at the moment its only defined in pure-register
		-- forms.
		if not (a and b and c) then
			return false, "LMOV must be properly qualified: 'LMOV R1,R2,R3,R4' (thats seg, offset, destseg, destoff)" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		local df, da, dv = parm(d)
		if not (af == 'register' and bf == 'register'
				and cf == 'register' and df == 'register') then
			return false, "LMOV only works with absolute registers" end
		if not (aa and ba and ca and da) then
			return false, "LMOV only works with absolute registers" end
		
		return string.char(0x24, reg_encode(av, bv), reg_encode(cv, dv))
	end;
	INC = function(a, b, c)
		-- the monopramic increment-by-one instruction., aka ADD 1, R, R
		-- at the moment its only defined for ACC.
		if not (a and not (b or c)) then 
			return false, "INC must be properly qualified: 'INC ACC'" end
		local af, aa, av = parm(a)
		if not (af == 'register' and aa and av=='ACC') then
			return false, "INC only supports absolute ACC as a destination" end
		
		return string.char(0x01)
	end;
	DEC = function(a, b, c)
		-- the monopramic decrement-by-one instruction., aka SUB 1, R, R
		-- at the moment its only defined for ACC.
		if not (a and not (b or c)) then 
			return false, "DEC must be properly qualified: 'DEC ACC'" end
		local af, aa, av = parm(a)
		if not (af == 'register' and aa and av=='ACC') then
			return false, "DEC only supports absolute ACC as a destination" end
		
		return string.char(0x1f)
	end;
	ADD = function(a, b, c)
		-- Addtion instruction, performs basic addition
		
		if not (a and b and c) then
			return false, "ADD must be properly qualified: 'ADD R1,R2,ACC'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "ADD only works with registers" end
		if not (aa and ba) then
			return false,  "ADD only works with absolute parms" end
		if not (cf == 'register' and ca and (cv=='ACC' or cv=='RET')) then 
			return false, "ADD only supports absolute ACC or RET as a destination" end
			
		if cv == 'ACC' then
			if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
				return string.char(0x09)
			else -- free-register form
				return string.char(0x06, reg_encode(av, bv))
			end
		elseif cv=='RET' then
			if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
				return string.char(0xa6)
			else -- free-register form
				return string.char(0xa4, reg_encode(av, bv))
			end
		end
		return false, "unhandled ADD form!"
	end;
	SUB = function(a, b, c)
		-- Subtraction instruction, performs basic subtraction
		if not (a and b and c) then
			return false, "SUB must be properly qualified: 'SUB R1,R2,ACC'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "SUB only works with registers" end
		if not (aa and ba) then
			return false, "SUB only works with absolute parms" end
		if not (cf == 'register' and ca and (cv=='ACC' or cv == 'RET')) then
			return false, "SUB only supports absolute ACC or RET as a destination" end
		
		if cv == 'ACC' then
			if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
				return string.char(0xa7)
			else -- free-register form
				return string.char(0x06, reg_encode(av, bv))
			end
		elseif cv=='RET' then
			if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
				return string.char(0xa5)
			else -- free-register form
				return string.char(0x20, reg_encode(av, bv))
			end
		end
	end;
	DIV = function(a, b, c)
		-- Integral, unsigned, division instruction.
		if not (a and b and c) then
			return false, "DIV must be properly qualified: 'DIV R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "DIV only works with registers" end
		if not (aa and ba) then
			return false,  "DIV only works with absolute parms" end
		if not (cf == 'register' and ca and (cv=='RET' or cv=='ACC')) then 
			return false, "DIV only supports absolute RET or ACC as a destination" end
		
		if cv == 'RET' then
			return string.char(0xa1, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1d, reg_encode(av, bv))
		end
	end;
	MUL = function(a, b, c)
		-- Integral, unsigned, Multiplication instruction.
		if not (a and b and c) then
			return false, "MUL must be properly qualified: 'MUL R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "MUL only works with registers" end
		if not (aa and ba) then
			return false,  "MUL only works with absolute parms" end
		if not (cf == 'register' and ca and (cv=='RET' or cv=='ACC')) then
			return false, "MUL only supports absolute RET or ACC as a destination." end
		
		if cv == 'RET' then
			return string.char(0x19, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1e, reg_encode(av, bv))
		end
	end;
	MOD = function(a, b, c)
		-- Integral, unsigned, Modulo (aka, remainder) instruction.
		if not (a and b and c) then
			return false, "MOD must be properly qualified: 'MOD R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "MOD only works with registers" end
		if not (aa and ba) then
			return false,  "MOD only works with absolute parms" end
		if not(cf == 'register' and ca and (cv=='RET' or cv=='ACC')) then
			return false, "MOD only supports absolute RET or ACC as a destination" end
		
		if cv == 'RET' then
			return string.char(0x1b, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1c, reg_encode(av, bv))
		end
	end;
	SHW = function(a, b, c)
		-- princibly a debugging instruction, show prints the name of a
		-- given register, followed by its value (as an unsigned byte)
		if not (a and not (b or c)) then
			return false, "SHW must be properly qualified: 'SHW R'" end
		local af, aa, av = parm(a)
		if not (af=='register' and aa) then 
			return false, "SHW only presently works for absolute registers" end
		
		if av=='A' then
			return string.char(0x0a)
		else
			return string.char(0xa1, reg_encode(av))
		end
	
	end;
	HLT = function(a, b, c)
		-- halt instruction, tells the machine to halt
		if not (not (a or b or c)) then
			return false, "HLT must be properly qualified: 'HLT'" end
		return string.char(0xff)
	end;
	SWP = function(a, b, c)
		-- swap, exchanges the values of parameters. only defined
		-- for registers
		if not ((a and b) and not c) then
			return false, "SWP must be properly qualified: 'SWP R1,R2'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (af == 'register' and bf == 'register') then
			return false, "SWP only works with registers" end
		if not (aa and ba) then
			return false,  "SWP only works with absolute parms" end
		
		if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
			return string.char(0x08)
		else -- free-register form
			return string.char(0x07, reg_encode(av, bv))
		end
		return false, "unhandled SWP form!"
	end;
	LES = function(a, b, c)
		-- less-than instruction, compares two values and stores the result
		-- in the third
		if not (a and b and c) then 
			return false, "LES must be properly qualified: 'LES R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "LES only works with absolute registers" end
		if not (aa and ba) then 
			return false, "LES only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "LES may only place its result in RET" end
		
		if (av == "A") and (bv == "B") then
			return string.char(0x0d)
		else -- free-register form
			
			--return string.char(0x0d)
			return false, "LES is only currently defined in the form LES .A,.B"
		end
		return false, "unhandled LES form!"
	end;
	GTR = function(a, b, c)
		-- greater-than instruction, compares two values and stores the
		-- result in the third
		if not (a and b and c) then
			return false, "GTR must be properly qualified: 'GTR R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "GTR only works with absolute registers" end
		if not (aa and ba) then
			return false, "GTR only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "GTR may only place its result in RET" end
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa3)
		else -- free-register form
			
			--return string.char(0x0d)
			return false, "GTR is only currently defined in the form GTR .A,.B"
		end
	end;
	EQL = function(a, b, c)
		-- equal-to instruction, compares two values and stores the
		-- result in the third
		if not (a and b and c) then
			return false, "EQL must be properly qualified: 'EQL R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "EQL only works with registers" end
		if not (aa and ba) then
			return false, "EQL only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "EQL may only place its result in RET" end
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa0)
		else -- free-register form
			
			return false, "EQL is only currently defined in the form EQL .A,.B"
		end
		return false, "unhandled EQL form!"
	end;
	GTE = function(a, b, c)
		-- greater-than-or-equal-to instruction, compares two values and
		-- stores the result in the third
		if not (a and b and c) then
			return false, "GTE must be properly qualified: 'GTE R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "GTE only works with registers" end
		if not (aa and ba) then
			return false, "GTE only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "GTE may only place its result in RET" end
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0x0d,0x08) -- swap, lessthan, swap
		else -- free-register form
			return false, "GTE is only currently defined in the form GTE .A,.B"
		end
		return false, "unhandled GRT form!"
	end;
	LTE = function(a, b, c)
		-- less-than-or-equal-to instruction, compares two values and stores
		-- the result in the third
		if not (a and b and c) then
			return false, "LTE must be properly qualified: 'LTE R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "LTE only works with registers" end
		if not (aa and ba) then
			return false, "LTE only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "LTE may only place its result in RET" end
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0xa3,0x08) -- swap, lessthan, swap
		else -- free-register form
			return false, "LTE is only currently defined in the form LTE .A,.B" 
		end
		return false, "unhandled LRT form!"
	end;
	JNZ = function(a, b, c)
		-- Jump-if-not-zero, the conditional jump instruction.
		if not (a and b and (not c)) then
			return false,  "JNZ must be properly qualified: 'JNZ RET,nn'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (aa and av=='RET') then
			return false, "JNZ may only test RET at this time." end
		if not (bf=='literal' or bf == 'symbol') then
			return false, "Only constant jump targets supported at this time." end
		if not (ba) then
			return false, "JNZ only supports absolute (inline) jump targets at this time." end
		
		if bf == 'literal' then 
			return string.char(0x0c, bv)
		elseif bf == 'symbol' then
			return string.char(0x0c, 0x00), true
		end		
		return false, "unhandled JNZ form!"
	end;
	JMP = function(a, b, c)
		-- jump instruction, transfers control to the given in-segment
		--  address
		if not (a and not (b or c)) then 
			return false, "JMP must be properly qualified: 'JMP nn'" end
		local af, aa, av = parm(a)
		if not (aa) then
			return false, "JMP only supports absolute jump targets at this time." end
		
		if af == 'literal' then 
			return string.char(0x0b, av)
		elseif af == 'symbol' then
			return string.char(0x0b, 0x00), true
		elseif af == 'register' then
			-- this is actually the free-register LJMP opcode, mapping the 
			-- first parm to SEG (the segment pointer) makes it effectively
			-- a free-register short-jump
			return string.char(0x25,  reg_encode('SEG', av))
		end		
		return false, "unhandled JMP form!"
	end;
	LJMP = function(a, b, c)
		-- long-jump instruction. transfers control to a given address in a
		-- (potentially) different segment.
		if not (a and b and not c) then 
			return false, "LJMP must be properly qualified: 'LJMP R1(seg), R2(offset)'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (aa and ba) then
			return false, "LJMP only supports absolute jump targets at this time." end
		if not (af == 'register' and bf == 'register') then
			return false, "LJMP only supports register jump targets at this time." end
		
		return string.char(0x25,  reg_encode(av, bv))
	end;
	LET = function(a, b, c)
		-- LET psudo-instruction, defines a sybol as the given value.
		if not ((a and b) and not c) then
			return false, "LET must be properly qualified: 'LET sym,nn'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (aa and ba) then
			return false, "Values may only be stored in build-time symbols" end
		if not (af=='symbol') then
			return false, "Values may only be stored in valid, unused, symbols" end
		if not (bf=='literal') then
			return false, "only literals, or known symbols may be stored" end

		symbols[a] = tonumber(b) or 0
		return ""
	end;
	LBL = function(a, b, c)
		-- Label psudo-instruction, sets the named symbols to the offset and 
		-- segment of the current location.
		if not (a and b) then
			return false, "LBL must be properly qualified: 'LBL adrsym,optsegsym" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (aa and ba) then
			return false, "Values may only be stored in build-time symbols" end
		if not (af=='symbol' and bf=='symbol') then
			return false, "Values may only be stored in valid, unused, symbols" end
		
		if av then symbols[av] = len%256 end
		if bv then symbols[bv] = math.floor(len/256) end
		return ""
	end;
	BYTE = function(a, b, c)
		-- BYTE psudo-instruction, defines the given symbols as the offset
		-- and segment of the current location, and encodes the third parm as
		-- a byte.
		-- thus, it effectively creates a named data storage position in
		--  memory.
		--
		-- this mess won't work properly when patching values...
		if not (a and b and c) then
			return false, "BYTE must be properly qualified: 'BYTE symbol,symbolseg,initialiser" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (aa and ba) then
			return false, "Values may only be stored in build-time symbols" end
		if not (af=='symbol' and bf=='symbol') then
			-- HAAAAACK!
			if not (av == (len%256) and bv == math.floor(len/256)) then
				return false, "Values may only be stored in valid, unused, symbols" end
			end
		if not (cf =='literal' or cf == 'symbol') then
			return false, "Only build-time values may be used as an initialiser" end
		
		if cf == 'literal' then 
			if not (cv == (cv % 256)) then
			return false, "a byte initialiser must be a positive value between 00..ff (0..255)" end
		end

		if av and bv then
			symbols[av] = len%256
			symbols[bv] = math.floor(len/256)
		end
		if cf=='literal' then
			return string.char(cv)
		else
			return string.char(00), true
		end
	end;
	MNZ = function(a, b, c)
		-- Move-not-zero: the conditional move instruction.
		-- operates similarly to JNZ, only it performs a move operation
		-- rather than a jump.
		if not (a and b and c) then
			return false, "MNZ must be properly qualified: 'MNZ R1,R2,nn'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)

		if not (af == 'register') then
			return false, "Can only test registers at this time" end
		if not (bf == 'register') then
			return false, "Can only conditionally move from registers at this time." end
		if not (cf=='literal' or cf == 'symbol') then
			return false, "Only constant set values supported at this time." end
		if not (aa and ba and ca) then
			return false, "MNZ only supports absolute values at this time." end
		
		if bf == 'register' then 
			return string.char(0x0e, reg_encode(av, bv), cv)
		elseif bf == 'symbol' then
			return string.char(0x0e, reg_encode(av, bv), 0x00), true
		end		
		return false, "unhandled MNZ form!"
	end;
	
	NOT = function(a, b, c)
		-- one's compliment negation instruction
		if not (a and b and (not c)) then
			return false, "NOT must be properly qualified: 'NOT R1, R2'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(a)
		if not (af == 'register' and bf == 'register') then
			return false, "NOT only works with absolute registers" end
		if not (aa and ba) then
			return false, "NOT only works with absolute registers" end
		
		return string.char(0x10, reg_encode(av, bv))
	end;
	AND = function(a, b, c)
		-- bitwise-AND instruction
		if not (a and b and c) then
			return false, "AND must be properly qualified: 'AND R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "AND only works with absolute registers" end
		if not (aa and ba) then
			return false, "AND only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "AND may only place its result in RET" end
		
		return string.char(0x11, reg_encode(av, bv))
	end;
	OR = function(a, b, c)
		-- bitwise-OR instruction
		if not (a and b and c) then
			return false, "OR must be properly qualified: 'OR R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "OR only works with absolute registers" end
		if not (aa and ba) then
			return false, "OR only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "OR may only place its result in RET" end
		
		return string.char(0x12, reg_encode(av, bv))
	end;
	XOR = function(a, b, c)
		-- bitwise-XOR instruction
		if not (a and b and c) then
			return false, "XOR must be properly qualified: 'XOR R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "XOR only works with absolute registers" end
		if not (aa and ba) then
			return false, "XOR only works with absolute registers" end
		
		return string.char(0x13, reg_encode(av, bv))
	end;
	SHL = function(a, b, c)
		-- Shift left instruction. shifts the bitpattern of the given value a
		-- number of positions given in the second parm, and stores the
		-- result in the third
		if not (a and b and c) then
			return false, "SHL must be properly qualified: 'SHL R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "SHL only works with absolute registers" end
		if not (aa and ba) then
			return false, "SHL only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "SHL may only place its result in RET" end
		
		return string.char(0x14, reg_encode(av, bv))
	end;
	SHR = function(a, b, c)
		-- Shift right instruction. shifts the bitpattern of the given value
		-- a number of positions given in the second parm, and stores the 
		-- result in the third
		if not (a and b and c) then
			return false, "SHR must be properly qualified: 'SHR R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "SHR only works with absolute registers" end
		if not (aa and ba) then
			return false, "SHR only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "SHR may only place its result in RET" end
		
		return string.char(0x15, reg_encode(av, bv))
	end;
	SRE = function(a, b, c)
		-- similar to SHR, save that it extends the sign-bit, thus
		-- (generally) preserving the sign of numbers in two's compliment
		if not (a and b and c) then
			return false, "SRE must be properly qualified: 'SRE R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "SRE only works with absolute registers" end
		if not (aa and ba) then
			return false, "SRE only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "SHR may only place its result in RET" end
		
		return string.char(0x16, reg_encode(av, bv))
	end;
	ROL = function(a, b, c)
		-- Roll Left instruction. Rolls are similar in effect to shifts,
		-- however the bits that "fall off" the end are simply replaced at
		-- the other end
		if not (a and b and c) then
			return false, "ROL must be properly qualified: 'ROL R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "ROL only works with absolute registers" end
		if not (aa and ba) then
			return false, "ROL only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "ROL may only place its result in RET" end
		
		return string.char(0x26, reg_encode(av, bv))
	end;
	ROR = function(a, b, c)
		-- Roll Right instruction. Rolls are similar in effect to shifts,
		-- however the bits that "fall off" the end are simply replaced at
		-- the other end
		if not (a and b and c) then
			return false, "ROR must be properly qualified: 'ROR R1,R2,RET'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		if not (af == 'register' and bf == 'register') then
			return false, "ROR only works with absolute registers" end
		if not (aa and ba) then
			return false, "ROR only works with absolute registers" end
		if not (ca and cv=='RET') then
			return false, "ROR may only place its result in RET" end
		
		return string.char(0x27, reg_encode(av, bv))
	end;
	IN = function(a, b, c)
		-- Device port read instruction. attempts to read a single value
		-- from the indicated device port, and places it in the location
		-- indicated by the second parm.
		if not (a and b and not c) then
			return false, "IN must be properly qualified: 'IN R1,R2'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (af == 'register' and bf == 'register') then
			return false, "IN only works with absolute registers" end
		if not (aa and ba) then
			return false, "IN only works with absolute registers" end
		
		return string.char(0x17, reg_encode(av, bv))
	end;
	OUT = function(a, b, c)
		-- Device port write instruction. attempts send a given byte of data
		-- to the indicated device port,
		if not (a and b and not c) then
			return false, "OUT must be properly qualified: 'OUT R1,R2'" end
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		if not (af == 'register' and bf == 'register') then
			return false, "OUT only works with absolute registers" end
		if not (aa and ba) then
			return false, "OUT only works with absolute registers" end
		
		return string.char(0x18, reg_encode(av, bv))
	end;

}

-----
-- processes the given string into a pre-parsed chunk, suitable for
--  further parsing.
function _M.scrub(s)
	local t = {}
	s = s..'\n'

	for l in string.gmatch(s, "[^\n]*\n") do
		s = string.match(l, "[^#\";]*")
		s = string.match(s, "[\t]*(.*)[\t]*")
		t[#t+1]=	s
	end
	assert(t, "scrub is returning nothing")
	return t
end

local scrub = _M.scrub

-----
-- loads the given file and returns a normalised chunk, suitable for
--  further parsing.
function _M.load(fname)
	local f, err = io.open(fname, 'rb')
	if not f then return nil, err end
	
	local t = scrub(f:read('*a'))
	f:close()
	
	assert(t, "load is returning nothing")
	return t
end

-----
-- takes a pre-parsed chunk, and returns a fully-assembled chunk as a
--  string, suitable for saving or loading into a machine.
function _M.parse(t, verbose)
	assert(t, "Parse has been given nothing")
	-- if its raw-text, then scrub it and continue.
	if type(t) == 'string' then t = _M.scrub(t) end
	local tos = tostring
	local chk,p = {}, {}
	
	symbols = {}
	
	-- phase I
	--  parse statements into operands and parms
	local op, a, b, c, d
	local match = "([&%[]?[%%$*]?[%w_']*[%]]?)[,%s]*(.*)"
	for i=1,#t do
		op, a = string.match(t[i], match)
		if a then
			a, b  = string.match(a, match) end
		if b then
			b, c  = string.match(b, match) end
		if c then
			c, d  = string.match(c, match) end

		if op == "" then op = nil end
		if a  == "" then a  = nil end
		if b  == "" then b  = nil end
		if c  == "" then c  = nil end
		if d  == "" then d  = nil end
		chk[i]={op=op, a=a, b=b, c=c, d=d}
	end
	
	--phase II
	-- encode into binary representations
	-- we take the prepared statements from the last phase, the first part
	-- should be the operator, for which we have a whole bunch of
	-- specialised encoder functions written. each encoder function takes
	-- the raw parms and attempts to parse and resolve them. if successful
	-- it will then attempt to reduce it to a specific binary representation.
	-- this can succeed, returning the encoded string, fail, producing false
	-- and an error message, or partially succeed, producing a partially 
	-- encoded chunk, and the value true (indicating that that chunk should
	-- be re-processed in the patching phase).
	local bin = {}
	local op, a, b, c, d, suc, r
	len = 0
	for i=1,#chk do
		op, a, b, c, d = chk[i].op,chk[i].a,chk[i].b,chk[i].c, chk[i].d
		
		if op then
			local lne = string.format("line %d: %s %s,%s,%s", i, tos(op), tos(a), tos(b), tos(c), tos(d))
			if not encoders[op] then
				local msg = ("[%s # Unknown instruction.]"):format(lne)
				if verbose then print(msg) end
				return false, msg
			end

			if verbose then print(string.format("%04x: %s %s, %s, %s",
				len, tos(op), tos(a), tos(b), tos(c))) end

			suc, r, patch = pcall(encoders[op], a, b, c, d)

			if suc then
				if not r then return false,	
					string.format("[line %d: %s %s,%s,%s # No valid reduction.]",
						 i, op, tos(a), tos(b), tos(c), tos(d))
				end
			else
				local lne = string.format("line %d: %s %s,%s,%s", i, tos(op), tos(a), tos(b), tos(c), tos(d))
				local msg = ("[%s # %s.]"):format(lne, tostring(r))
				if verbose then print(msg) end
				return false, msg
			end

			if patch then p[#p+1] = i end
			
			bin[i] = r or ""

			len = len + #r
		end
		chk[i].len = len -- HAAAAACK

		if bin[i] == nil then bin[i] = "" end
	end
	
	--phase III
	-- now (re)do patch points
	-- during initial encoding some chunks were (possibly) markede for
	-- patching. this means there wasn't enough info at the time to give a
	-- proper encoding, so now we give them one last chance to resolve.
	-- which is usually enough for properly written code.
	local i
	for j=1,#p do
		i = p[j]
		op, a, b, c, d, len = chk[i].op,chk[i].a,chk[i].b,chk[i].c,chk[i].d,chk[i].len
		if verbose then print(string.format("Patching %s %s, %s", tos(op), tos(a), tos(b))) end
		len = chk[i].len -- HAAAAACK
		r, patch = encoders[op](a, b, c)
		if r and not patch then
			bin[i] = r
			len = len + #r
		else
			return false, ("[line %d: %s %s,%s # Failed to patch symbol.]"):format(i, op, tos(a), tos(b))
		end
		
	end
	
	-- finally...
	-- concatinate the encoded fragments into a single chunk, and return it.
	return table.concat(bin)
end


-- possibly clever shit
if arg and arg[0] and (arg[0]=='asm.lua' or arg[0]=='asm') then
	print("Begining")

	math.randomseed(os.time())
	local function tmpnam() return string.format("x%04x", math.random(0xffff+1)-1) end
	
	local inf, outf, err = arg[1], arg[2], ""
	assert(inf, "must specify a file to load")
	print(string.format("Loading '%s'", inf))
	local chk, err = _M.load(inf)

	if not outf then outf = (string.match(inf, "[^%.]*") or tmpnam())..".crap" end

	print(string.format("Writting '%s'", tostring(outf)))
	
	outf, err = io.open(outf, "wb")
	assert(outf, err)
	chk, err = _M.parse(chk, true)
	
	if chk then
		outf:write(chk)
	else
		print("Failed: "..tostring(err))
		return 1
	end
	outf:close()
	
	io.write('{')
	for i=1,#chk do io.write(string.format("0x%02x;", chk:byte(i))) end
	print('}')
	return 0
end
	

-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

