local _M = {_NAME="asm", number=0}

local _MT = {__index=_M}

local symbols, len -- needed for parsing

local regnum = {PRM=0x0, A  =0x1, B  =0x2, ACC=0x3, RET=0x4,
                SIG=0xd, SEG=0xe, IP =0xf}
local regs = {'A', 'B', 'ACC', 'RET', 'SEG', 'IP'} 

local function reg_encode(a, b)
	a = a or 'PRM'
	b = b or 'PRM'

	assert(regnum[a] and regnum[b], "Invalid Register passed to reg_encode")
	return (regnum[a]*16) + regnum[b]
end

local function parm(p)
	assert(p, "Can't check nil parms!")
	p = string.match(p, "%S+")
	local form, abs, value = 'unk', not (p:sub(1, 1) == '&'), p	
	p = p:match("&?(.*)")
	
	for i=1,#regs do
		form = regs[i] == p and 'register' or form
	end
	
	if form ~= 'register' then 
		value = tonumber(p)
		form = 'literal'
		
		if value == nil then
			value = symbols[p]
			if value then 
				return form, abs, value
			else
				--assert(value, string.format("Okay, I give up. whats '%s'?", tostring(p)))
				form = 'symbol'
			end
				
			
		end
	end
	
	return form, abs, value
end

local encoders = {
	NOP = function(a, b)
		assert(not (a or b), "NOP has no parms")
		return string.char(0x00)	
	end;
	MOV = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		
		if af == 'register' and bf == 'register' then -- free-register form
			if aa == true and ba == true then -- pure register form
				return string.char(0x0f, reg_encode(av, bv))
			elseif aa == false and ba == false then --pure addressed register form
				return string.char(0x02, reg_encode(av, bv))
			end
		end
		
		if af == 'literal' and bf=='register' then
			if aa == true and ba == true then
				assert(bv=="A", "MOV litteral to (not A) not implemented")
				return string.char(0x05, tonumber(av))
			elseif aa == false and ba == true then -- mixed get form
				return string.char(0x04, tonumber(av))
			end
		end
		if af == 'register' and bf=='literal' then
			if aa == true and ba == false then -- mixed push/set form
				assert(bv=="A", "MOV litteral from (not A) not implemented")
				return string.char(0x03, tonumber(av))
			end
		end
	end;
	ADD = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "ADD only works with registers")
		assert(aa and ba,  "ADD only works with absolute parms")
		
		if (av == "A" or av == "B") and (bv == "A" or bv == "B") then
			return string.char(0x09)
		else -- free-register form
			return string.char(0x06, reg_encode(av, bv))
		end
		error("unhandled ADD form!")
	end;
	DIV = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "DIV only works with registers")
		assert(aa and ba,  "DIV only works with absolute parms")
		
		return string.char(0xa1, reg_encode(av, bv))
	end;
	SHW = function(a, b)
		local af, aa, av = parm(a)
		assert(af=='register' and aa, "SHW only presently works for absolute registers")
		
		if av=='A' then
			return string.char(0x0a)
		else
			return string.char(0xa1, reg_encode(av))
		end
	
	end;
	HLT = function(a, b)
--		assert(not (a or b), "HLT has no parms")
		return string.char(0xff)
	end;
	SWP = function(a, b)
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
	LES = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x0d)
		else -- free-register form
			
			--return string.char(0x0d)
			error("LES is only currently defined in the form LES .A,.B")
		end
		error("unhandled LES form!")
	end;
	GTR = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "GTR only works with absolute registers")
		assert(aa and ba, "GTR only works with absolute registers")
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa3)
		else -- free-register form
			
			--return string.char(0x0d)
			error("GTR is only currently defined in the form GTR .A,.B")
		end
	end;
	EQL = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "EQL only works with registers")
		assert(aa and ba, "EQL only works with absolute registers")
		
		if (av == "A") and (bv == "B") then
			return string.char(0xa0)
		else -- free-register form
			
			error("EQL is only currently defined in the form EQL .A,.B")
		end
		error("unhandled LES form!")
	end;
	GTE = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "GTE only works with registers")
		assert(aa and ba, "GTE only works with absolute registers")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0x0d,0x08) -- swap, lessthan, swap
		else -- free-register form
			error("GTE is only currently defined in the form GTE .A,.B")
		end
		error("unhandled GRT form!")
	end;
	LTE = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LTE only works with registers")
		assert(aa and ba, "LTE only works with absolute registers")
		
		if (av == "A") and (bv == "B") then
			return string.char(0x08,0xa3,0x08) -- swap, lessthan, swap
		else -- free-register form
			error("GTE is only currently defined in the form LTE .A,.B")
		end
		error("unhandled LRT form!")
	end;
	LBL = function(a, b)
		assert((a or b), "LBL requires parms")
		
		if a then symbols[a] = len%256 end
		if b then symbols[b] = math.floor(len/256) end
		return ""
	end;
	JNZ = function(a, b)
		assert(a or b, "JNZ requires parms.")
		assert(not b, "JNZ only accepts one parm.")
		local af, aa, av = parm(a)
		assert(af=='literal' or af == 'symbol', "Only constant jump targets supported at this time.")
		assert(aa, "JNZ only supports absolute (inline) jump targets at this time.")
		
		if af == 'literal' then 
			return string.char(0x0c, av)
		elseif af == 'symbol' then
			return string.char(0x0c, 0x00), true
		end		
		error("unhandled JNZ form!")
	end;
	JMP = function(a, b)
		assert(a or b, "JMP requires parms.")
		assert(not b, "JMP only accepts one parm.")
		local af, aa, av = parm(a)
		assert(af=='literal' or af == 'symbol', "Only constant jump targets supported at this time.")
		assert(aa, "JMP only supports absolute (inline) jump targets at this time.")
		
		if af == 'literal' then 
			return string.char(0x0b, av)
		elseif af == 'symbol' then
			return string.char(0x0b, 0x00), true
		end		
		error("unhandled JNZ form!")
	end;
	LET = function(a, b)
		assert((a or b), "LBL requires parms")
		
		symbols[a] = tonumber(b) or 0
		return ""
	end;
	MNZ = function(a, b)
		assert(a or b, "MNZ requires parms.")
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register', "Can only conditionally move from registers at this time.")
		assert(bf=='literal' or bf == 'symbol', "Only constant set values supported at this time.")
		assert(aa and ba, "MNZ only supports absolute values at this time.")
		
		if bf == 'register' then 
			return string.char(0x0e, reg_encode('RET', av), bv)
		elseif bf == 'symbol' then
			return string.char(0x0e, reg_encode('RET', av), 0x00), true
		end		
		error("unhandled MNZ form!")
	end;
	
	NOT = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		
		return string.char(0x10, reg_encode(av, bv))
	end;
	AND = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		
		return string.char(0x11, reg_encode(av, bv))
	end;
	OR = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		
		return string.char(0x12, reg_encode(av, bv))
	end;
	XOR = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "LES only works with absolute registers")
		assert(aa and ba, "LES only works with absolute registers")
		
		return string.char(0x13, reg_encode(av, bv))
	end;
	SHL = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "SHL only works with absolute registers")
		assert(aa and ba, "SHL only works with absolute registers")
		
		return string.char(0x14, reg_encode(av, bv))
	end;
	SHR = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "SHR only works with absolute registers")
		assert(aa and ba, "SHR only works with absolute registers")
		
		return string.char(0x15, reg_encode(av, bv))
	end;
	SRE = function(a, b)
		local af, aa, av = parm(a)
		local bf, ba, bv = parm(b)
		assert(af == 'register' and bf == 'register', "SRE only works with absolute registers")
		assert(aa and ba, "SRE only works with absolute registers")
		
		return string.char(0x16, reg_encode(av, bv))
	end;

}

-----
-- processes the given string into a pre-parsed chunk, suitable for
--  further parsing.
function _M.scrub(s)
	local t = {}
	s = s..'\n'
--	s = string.gsub(s, "\n\n", "\n") --in retrospect, keep lines
	
	for l in string.gmatch(s, "[^\n]*\n") do
		s = string.match(l, "[^#;]*")
		if s ~= "\n" and s ~= "" then 
			t[#t+1]=	s
		end
	end
	
	return t
end

local scrub = _M.scrub

-----
-- loads the given file and returns a normalised chunk, suitable for
--  further parsing.
function _M.load(fname)
	local f, err = io.open(fname)
	if not f then return nil, err end
	
	local t = scrub(f:read('*a'))
	f:close()
	
	return t
end

-----
-- takes a pre-parsed chunk, and returns a fully-assembled chunk as a
--  string, suitable for saving or loading into a machine.
function _M.parse(t)
	local tos = tostring
	local c,p = {}, {}
	
	symbols = {}
	
	--  parse statements into operands and parms
	local op, a, b
	for i=1,#t do
		op, a = string.match(t[i], "(%u*) *(.*)")
		--print(a)
		a, b = string.match(a, "([^%p%s]*)[,%s]?[,%s]?([^%p%s]*)")
		--print(a, b)
		if op == "" then op = nil end
		if a  == "" then a  = nil end
		if b  == "" then b  = nil end
		c[i]={op=op, a=a, b=b}
	end
	
	-- encode into binary representations
	local bin = {}
	local op, a, b, r
	len = 0
	for i=1,#c do
		op, a, b = c[i].op,c[i].a,c[i].b
		if op then
			assert(encoders[op], ("[line %d: %s %s,%s # Unknown instruction.]"):format(i, op, tos(a), tos(b)))
			print(string.format("%s %s, %s", tos(op), tos(a), tos(b)))
			r, patch = encoders[op](a, b)
			assert(r, ("[line %d: %s %s,%s # No valid reduction.]"):format(i, op, tos(a), tos(b)))

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
		op, a, b = c[i].op,c[i].a,c[i].b
		print(string.format("Patching %s %s, %s", tos(op), tos(a), tos(b)))
		r, patch = encoders[op](a, b)
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
if arg and arg[0] then
	print("Begining")

	math.randomseed(os.time())
	local function tmpnam() return string.format("x%04x", math.random(0xffff+1)-1) end
	
	local inf, outf, err = arg[1], arg[2], ""
	assert(inf, "must specify a file to load")
	print(string.format("Loading '%s'", inf))
	local chk = _M.load(inf)

	if not outf then outf = (string.match(inf, "[^%.]*") or tmpnam())..".crap" end

	print(string.format("Writting '%s'", tostring(outf)))
	
	outf, err = io.open(outf, "w")
	assert(outf, err)
	chk = _M.parse(chk)
	outf:write(chk)
	outf:close()
	
	io.write('{')
	for i=1,#chk do io.write(string.format("0x%02x;", chk:byte(i))) end
	print('}')
end
	

-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

