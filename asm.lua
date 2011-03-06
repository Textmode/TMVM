local _M = {_NAME="asm", number=0}

local _MT = {__index=_M}

local symbols, len -- needed for parsing

local regnum = {PRM=0x0, A  =0x1, B=0x2, C=0x3, D=0x4, ACC=0x5, RET=0x6,
                SIG=0xd, SEG=0xe, IP =0xf}
local regs = {'A', 'B', 'C', 'D', 'ACC', 'RET', 'SIG', 'SEG', 'IP'} 

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
	elseif n:sub(1,1) == "O" then  -- Octal
		return tonumber(n:sub(2), 8)
	elseif n:match("'(.)'") then  -- char-literal
		return string.byte(n:match("'(.)'"))
	else  -- doesn't seem to be anything special, decimal?
		return tonumber(n) 
	end
end

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
				--assert(value, string.format("Okay, I give up. whats '%s'?", tostring(p)))
				form = 'symbol'
				value = p
			end
				
			
		end
	end
	
	return form, abs, value
end

local encoders = {
	NOP = function(a, b, c)
		assert(not (a or b or c), "NOP must be properly qualified: 'NOP'")
		return string.char(0x00)	
	end;
	MOV = function(a, b, c)
		assert(a and b and not c, "MOV must be properly qualified: 'MOV P1,P2'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(not ((af=='literal' or af=='symbol') and (bf=='literal' or bf=='symbol')),
			 "All move operations must involve a register, sorry.")
		
		if af == 'register' and bf == 'register' then -- free-register form
			if aa == true and ba == true then -- pure register form
				return string.char(0x0f, reg_encode(av, bv))
			elseif aa == false and ba == false then --pure addressed register form
				return string.char(0x02, reg_encode(av, bv))
			end
		end
		
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
		assert(a and b and c, "SRE must be properly qualified: 'SRE R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		local df, da, dv = parm(d)
		assert(af == 'register' and bf == 'register'
			and cf == 'register' and df == 'register',
				"LMOV only works with absolute registers")
		assert(aa and ba and ca and da, "LMOV only works with absolute registers")
		
		return string.char(0x24, reg_encode(av, bv), reg_encode(cv, dv))
	end;
	INC = function(a, b, c)
		assert(a and not (b or c), "INC must be properly qualified: 'INC ACC'")
		local af, aa, av = parm(a)
		assert(af == 'register' and aa and av=='ACC', "INC only supports absolute ACC as a destination")
		
		return string.char(0x01)
	end;
	DEC = function(a, b, c)
		assert(a and not (b or c), "DEC must be properly qualified: 'DEC ACC'")
		local af, aa, av = parm(a)
		assert(af == 'register' and aa and av=='ACC', "DEC only supports absolute ACC as a destination")
		
		return string.char(0x1f)
	end;
	ADD = function(a, b, c)
		assert(a and b and c, "ADD must be properly qualified: 'ADD R1,R2,ACC'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "ADD only works with registers")
		assert(aa and ba,  "ADD only works with absolute parms")
		assert(cf == 'register' and ca and (cv=='ACC' or cv=='RET'), "ADD only supports absolute ACC or RET as a destination")
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
		error("unhandled ADD form!")
	end;
	SUB = function(a, b, c)
		assert(a and b and c, "SUB must be properly qualified: 'SUB R1,R2,ACC'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "SUB only works with registers")
		assert(aa and ba,  "SUB only works with absolute parms")
		assert(cf == 'register' and ca and cv=='ACC', "SUB only supports absolute ACC as a destination")
		
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
		assert(a and b and c, "DIV must be properly qualified: 'DIV R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "DIV only works with registers")
		assert(aa and ba,  "DIV only works with absolute parms")
		assert(cf == 'register' and ca and (cv=='RET' or cv=='ACC'), "DIV only supports absolute RET or ACC as a destination")
		
		if cv == 'RET' then
			return string.char(0xa1, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1d, reg_encode(av, bv))
		end
	end;
	MUL = function(a, b, c)
		assert(a and b and c, "MUL must be properly qualified: 'MUL R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "MUL only works with registers")
		assert(aa and ba,  "MUL only works with absolute parms")
		assert(cf == 'register' and ca and (cv=='RET' or cv=='ACC'), "MUL only supports absolute RET or ACC as a destination.")
		
		if cv == 'RET' then
			return string.char(0x19, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1e, reg_encode(av, bv))
		end
	end;
	MOD = function(a, b, c)
		assert(a and b and c, "MOD must be properly qualified: 'MOD R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "MOD only works with registers")
		assert(aa and ba,  "MOD only works with absolute parms")
		assert(cf == 'register' and ca and (cv=='RET' or cv=='ACC'), "MOD only supports absolute RET or ACC as a destination")
		
		if cv == 'RET' then
			return string.char(0x1b, reg_encode(av, bv))
		else -- cv == ACC
			return string.char(0x1c, reg_encode(av, bv))
		end
	end;
	SHW = function(a, b, c)
		assert(a and not (b or c), "SHW must be properly qualified: 'SHW R'")
		local af, aa, av = parm(a)
		assert(af=='register' and aa, "SHW only presently works for absolute registers")
		
		if av=='A' then
			return string.char(0x0a)
		else
			return string.char(0xa1, reg_encode(av))
		end
	
	end;
	HLT = function(a, b, c)
		assert(not (a or b or c), "HLT must be properly qualified: 'HLT'")
		return string.char(0xff)
	end;
	SWP = function(a, b, c)
		assert((a and b) and not c, "SWP must be properly qualified: 'SWP R1,R2'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "SWP only works with registers")
		assert(aa and ba,  "SWP only works with absolute parms")
		
		if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
			return string.char(0x08)
		else -- free-register form
			return string.char(0x07, reg_encode(av, bv))
		end
		error("unhandled SWP form!")
	end;
	LES = function(a, b, c)
		assert(a and b and c, "LES must be properly qualified: 'LES R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		assert(ca and cv=='RET', "LES may only place its result in RET")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x0d)
		else -- free-register form
			
			--return string.char(0x0d)
			error("LES is only currently defined in the form LES .A,.B")
		end
		error("unhandled LES form!")
	end;
	GTR = function(a, b, c)
		assert(a and b and c, "GTR must be properly qualified: 'GTR R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "GTR only works with absolute registers")
		assert(aa and ba, "GTR only works with absolute registers")
		assert(ca and cv=='RET', "GTR may only place its result in RET")
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa3)
		else -- free-register form
			
			--return string.char(0x0d)
			error("GTR is only currently defined in the form GTR .A,.B")
		end
	end;
	EQL = function(a, b, c)
		assert(a and b and c, "EQL must be properly qualified: 'EQL R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "EQL only works with registers")
		assert(aa and ba, "EQL only works with absolute registers")
		assert(ca and cv=='RET', "EQL may only place its result in RET")
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa0)
		else -- free-register form
			
			error("EQL is only currently defined in the form EQL .A,.B")
		end
		error("unhandled EQL form!")
	end;
	GTE = function(a, b, c)
		assert(a and b and c, "GTE must be properly qualified: 'GTE R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "GTE only works with registers")
		assert(aa and ba, "GTE only works with absolute registers")
		assert(ca and cv=='RET', "GTE may only place its result in RET")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0x0d,0x08) -- swap, lessthan, swap
		else -- free-register form
			error("GTE is only currently defined in the form GTE .A,.B")
		end
		error("unhandled GRT form!")
	end;
	LTE = function(a, b, c)
		assert(a and b and c, "LTE must be properly qualified: 'LTE R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "LTE only works with registers")
		assert(aa and ba, "LTE only works with absolute registers")
		assert(ca and cv=='RET', "LTE may only place its result in RET")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0xa3,0x08) -- swap, lessthan, swap
		else -- free-register form
			error("LTE is only currently defined in the form LTE .A,.B")
		end
		error("unhandled LRT form!")
	end;
	JNZ = function(a, b, c)
		assert(a and b and (not c), "JNZ must be properly qualified: 'JNZ RET,nn'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(aa and av=='RET', "JNZ may only test RET at this time.")
		assert(bf=='literal' or bf == 'symbol', "Only constant jump targets supported at this time.")
		assert(ba, "JNZ only supports absolute (inline) jump targets at this time.")
		
		if bf == 'literal' then 
			return string.char(0x0c, bv)
		elseif bf == 'symbol' then
			return string.char(0x0c, 0x00), true
		end		
		error("unhandled JNZ form!")
	end;
	JMP = function(a, b, c)
		assert(a and not (b or c), "JMP must be properly qualified: 'JMP nn'")
		local af, aa, av = parm(a)
		assert(aa, "JMP only supports absolute jump targets at this time.")
		
		if af == 'literal' then 
			return string.char(0x0b, av)
		elseif af == 'symbol' then
			return string.char(0x0b, 0x00), true
		elseif af == 'register' then
			return string.char(0x25,  reg_encode('SEG', av))
		end		
		error("unhandled JMP form!")
	end;
	LJMP = function(a, b, c)
		assert(a and b and not c, "LJMP must be properly qualified: 'LJMP R1(seg), R2(offset)'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(aa and ba, "LJMP only supports absolute jump targets at this time.")
		assert(af == 'register' and bf == 'register',
			"LJMP only supports register jump targets at this time.")
		
		return string.char(0x25,  reg_encode(av, bv))
		--error("unhandled LJMP form!")
	end;
	LET = function(a, b, c)
		assert((a and b) and not c, "LET must be properly qualified: 'LET sym,nn'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(aa and ba, "Values may only be stored in build-time symbols")
		assert(af=='symbol', "Values may only be stored in valid, unused, symbols")
		assert(bf=='literal', "only literals, or known symbols may be stored")

		symbols[a] = tonumber(b) or 0
		return ""
	end;
	LBL = function(a, b, c)
		assert(a and b, "LBL must be properly qualified: 'LBL adrsym,optsegsym")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(aa and ba, "Values may only be stored in build-time symbols")
		assert(af=='symbol' and bf=='symbol', "Values may only be stored in valid, unused, symbols")
		
		if av then symbols[av] = len%256 end
		if bv then symbols[bv] = math.floor(len/256) end
		return ""
	end;
	-- this mess won't work properly when patching values...
	BYTE = function(a, b, c)
		assert(a and b and c, "BYTE must be properly qualified: 'BYTE symbol,symbolseg,initialiser")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(aa and ba, "Values may only be stored in build-time symbols")
		assert(af=='symbol' and bf=='symbol', "Values may only be stored in valid, unused, symbols")
		assert(cf =='literal' or cf == 'symbol', "Only build-time values may be used as an initialiser")

		if av and bv then
			symbols[av] = len%256
			symbols[bv] = math.floor(len/256)
		end
		if cf=='literal' then
			return string.char(cv%256)
		else
			return string.char(00), true
		end
	end;
	MNZ = function(a, b, c)
		assert(a and b and c, "MNZ must be properly qualified: 'MNZ R1,R2,nn'")
		local af, aa, av
		local bf, ba, bv
		local cf, ca, cv

		if a and b and c then
			af, aa, av = parm(a)
			bf, ba, bv = parm(b)
			cf, ca, cv = parm(c)
		else
			af, aa, av = 'register', true, 'RET'
			bf, ba, bv = parm(a)
			cf, ca, cv = parm(b)
		end
		assert(af == 'register', "Can only test registers at this time")
		assert(bf == 'register', "Can only conditionally move from registers at this time.")
		assert(cf=='literal' or cf == 'symbol', "Only constant set values supported at this time.")
		assert(aa and ba and ca, "MNZ only supports absolute values at this time.")
		
		if bf == 'register' then 
			return string.char(0x0e, reg_encode(av, bv), cv)
		elseif bf == 'symbol' then
			return string.char(0x0e, reg_encode(av, bv), 0x00), true
		end		
		error("unhandled MNZ form!")
	end;
	
	NOT = function(a, b, c)
		assert(a and b and (not c), "NOT must be properly qualified: 'NOT R1, R2'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(a)
		assert(af == 'register' and bf == 'register', "NOT only works with absolute registers")
		assert(aa and ba, "NOT only works with absolute registers")
		
		return string.char(0x10, reg_encode(av, bv))
	end;
	AND = function(a, b, c)
		assert(a and b and c, "AND must be properly qualified: 'AND R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "AND only works with absolute registers")
		assert(aa and ba, "AND only works with absolute registers")
		assert(ca and cv=='RET', "AND may only place its result in RET")
		
		return string.char(0x11, reg_encode(av, bv))
	end;
	OR = function(a, b, c)
		assert(a and b and c, "OR must be properly qualified: 'OR R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "OR only works with absolute registers")
		assert(aa and ba, "OR only works with absolute registers")
		assert(ca and cv=='RET', "OR may only place its result in RET")
		
		return string.char(0x12, reg_encode(av, bv))
	end;
	XOR = function(a, b, c)
		assert(a and b and c, "XOR must be properly qualified: 'XOR R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "XOR only works with absolute registers")
		assert(aa and ba, "XOR only works with absolute registers")
		
		return string.char(0x13, reg_encode(av, bv))
	end;
	SHL = function(a, b, c)
		assert(a and b and c, "SHL must be properly qualified: 'SHL R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "SHL only works with absolute registers")
		assert(aa and ba, "SHL only works with absolute registers")
		assert(ca and cv=='RET', "SHL may only place its result in RET")
		
		return string.char(0x14, reg_encode(av, bv))
	end;
	SHR = function(a, b, c)
		assert(a and b and c, "SHR must be properly qualified: 'SHR R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "SHR only works with absolute registers")
		assert(aa and ba, "SHR only works with absolute registers")
		assert(ca and cv=='RET', "SHR may only place its result in RET")
		
		return string.char(0x15, reg_encode(av, bv))
	end;
	SRE = function(a, b, c)
		assert(a and b and c, "SRE must be properly qualified: 'SRE R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "SRE only works with absolute registers")
		assert(aa and ba, "SRE only works with absolute registers")
		assert(ca and cv=='RET', "SHR may only place its result in RET")
		
		return string.char(0x16, reg_encode(av, bv))
	end;
	ROL = function(a, b, c)
		assert(a and b and c, "ROL must be properly qualified: 'ROL R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "ROL only works with absolute registers")
		assert(aa and ba, "ROL only works with absolute registers")
		assert(ca and cv=='RET', "ROL may only place its result in RET")
		
		return string.char(0x26, reg_encode(av, bv))
	end;
	ROR = function(a, b, c)
		assert(a and b and c, "ROR must be properly qualified: 'ROR R1,R2,RET'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		local cf, ca, cv = parm(c)
		assert(af == 'register' and bf == 'register', "ROR only works with absolute registers")
		assert(aa and ba, "ROR only works with absolute registers")
		assert(ca and cv=='RET', "ROR may only place its result in RET")
		
		return string.char(0x27, reg_encode(av, bv))
	end;
	IN = function(a, b, c)
		assert(a and b and not c, "IN must be properly qualified: 'IN R1,R2'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "IN only works with absolute registers")
		assert(aa and ba, "IN only works with absolute registers")
		
		return string.char(0x17, reg_encode(av, bv))
	end;
	OUT = function(a, b, c)
		assert(a and b and not c, "OUT must be properly qualified: 'OUT R1,R2'")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "OUT only works with absolute registers")
		assert(aa and ba, "OUT only works with absolute registers")
		
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
		if s ~= "\n" and s:match("(%S+)") then 
			t[#t+1]=	s
		end
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
	if type(t) == 'string' then t = _M.scrub(t) end
	local tos = tostring
	local chk,p = {}, {}
	
	symbols = {}
	
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
	
	-- encode into binary representations
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
				assert(r, ("[line %d: %s %s,%s,%s # No valid reduction.]"):
					format(i, op, tos(a), tos(b), tos(c), tos(d)))
			else
				local lne = string.format("line %d: %s %s,%s,%s", i, tos(op), tos(a), tos(b), tos(c), tos(d))
				local msg = ("[%s # %s.]"):format(lne, tostring(r))
				if verbose then print(msg) end
				return false, msg
			end

			bin[i] = r or ""
			len = len + #r
			--print("YEHAW!", patch)
			if patch then p[#p+1] = i end
		end
	end
	
	-- now (re)do patch points
	local i
	for j=1,#p do
		i = p[j]
		op, a, b, c = chk[i].op,chk[i].a,chk[i].b,chk[i].c
		if verbose then print(string.format("Patching %s %s, %s", tos(op), tos(a), tos(b))) end
		r, patch = encoders[op](a, b, c)
		if r then
			bin[i] = r
			len = len + #r
		else
			bin[i]=""
		end
		
		assert(not patch, ("[line %d: %s %s,%s # Failed to patch symbol.]"):format(i, op, tos(a), tos(b)))
		
	end
	
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
		print("Failed.")
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

