-------------------------------------------------------------------------
---- Test functions and storage.

local test = {section={name="", num=0}, subsection={name="", num=0}, test={num=0}}

setmetatable(test, test)

function test:verse(t, short)
	if short then
		return string.format("[%d.%d.%d]", self.section.num, self.subsection.num, self.test.num)
	else
		return string.format("[%d.%d.%d - %s]", self.section.num, self.subsection.num, self.test.num, tostring(t))
	end
end

function test:__call(name, case, expected)
	self.test.num = self.test.num+1
	local t, ret = loadstring(case, self:verse(name, true))
	if t then
		t, ret = pcall(t)
	else
		print(string.format("%s failed. Uncompilable. \n\t%s\n\t%s",
			self:verse(name), tostring(case), tostring(err)))
		return false
	end
	
	if not t then --runtime error
		print(string.format("%s failed. Error. \n\t%s\n\t%s",
			self:verse(name), tostring(case), tostring(ret)))
		return false
	end
	
	if ret == expected then
		print(string.format("%s passed.", self:verse(name)))
		return true
	else
		print(string.format("%s Failed. expected '%s', got '%s' instead.",
			self:verse(name), tostring(expected), tostring(ret)))
		return false
	end
	return false
end

function test:newsection(name)
	self.section = {name=tostring(name), num=self.section.num+1}
	self.subsection = {name="", num=0}
	self.test.num=0
	print(string.format("[%d] - %s", self.section.num, tostring(name)))
end

function test:newsubsection(name)
	self.subsection = {name=tostring(name), num=self.subsection.num+1}
	self.test.num=0
	print(string.format("[%d.%d] - %s", self.section.num, self.subsection.num, tostring(name)))
end

-------------------------------------------------------------------------
---- Actual tests

test:newsection("Test harness")
test:newsubsection("Core")
	test("true == true", 
		"return true", true)
	test("false == false", 
		"return false", false)
	test("true == false", 
		"return true", false)
	test("42 == 42", 
		"return 42", 42)
	test("type(42) == 'number'", 
		"return type(42)", 'number')
	test("false == nil", 
		"return false", nil)
	test("nil == false", 
		"return nil", false)

-------------------------------------------------------------------------
test:newsection("Bitfield")

bitfield = require 'bitfield'
test:newsubsection("Core")
	test("Creation, indexing, and printing", 
		"local b = bitfield:new(); return tostring(b)", '0000000000000000b')
	test("Initialisation", 
		"local b = bitfield:new(42); return b.value", 42)
	test("Indexing and setting", 
		"local b = bitfield:new(0); b[2]=1; b[1]=1 return b.value", 3)
	test("widths", 
		"local b = bitfield:new(4, 3); return tostring(b)", '100b')

test:newsubsection("NOT")
	test("NOT 0 (simple)",
		"local b = bitfield:new(0, 1); b:NOT(); return tostring(b)", '1b')
	test("NOT 1 (simple)",
		"local b = bitfield:new(1, 1); b:NOT(); return tostring(b)", '0b')
test:newsubsection("AND")
	test("0 AND 0 (simple)",
		"local b = bitfield:new(0, 1); b:AND(0); return tostring(b)", '0b')
	test("0 AND 1 (simple)",
		"local b = bitfield:new(0, 1); b:AND(1); return tostring(b)", '0b')
	test("1 AND 0 (simple)",
		"local b = bitfield:new(1, 1); b:AND(0); return tostring(b)", '0b')
	test("1 AND 1 (simple)",
		"local b = bitfield:new(1, 1); b:AND(1); return tostring(b)", '1b')

test:newsubsection("OR")
	test("0 OR 0 (simple)",
		"local b = bitfield:new(0, 1); b:OR(0); return tostring(b)", '0b')
	test("0 OR 1 (simple)",
		"local b = bitfield:new(0, 1); b:OR(1); return tostring(b)", '1b')
	test("1 OR 0 (simple)",
		"local b = bitfield:new(1, 1); b:OR(0); return tostring(b)", '1b')
	test("1 OR 1 (simple)",
		"local b = bitfield:new(1, 1); b:OR(1); return tostring(b)", '1b')

test:newsubsection("XOR")
	test("0 XOR 0 (simple)",
		"local b = bitfield:new(0, 1); b:XOR(0); return tostring(b)", '0b')
	test("0 XOR 1 (simple)",
		"local b = bitfield:new(0, 1); b:XOR(1); return tostring(b)", '1b')
	test("1 XOR 0 (simple)",
		"local b = bitfield:new(1, 1); b:XOR(0); return tostring(b)", '1b')
	test("1 XOR 1 (simple)",
		"local b = bitfield:new(1, 1); b:XOR(1); return tostring(b)", '0b')

test:newsubsection("NAND")
	test("0 NAND 0 (simple)",
		"local b = bitfield:new(0, 1); b:NAND(0); return tostring(b)", '1b')
	test("0 NAND 1 (simple)",
		"local b = bitfield:new(0, 1); b:NAND(1); return tostring(b)", '1b')
	test("1 NAND 0 (simple)",
		"local b = bitfield:new(1, 1); b:NAND(0); return tostring(b)", '1b')
	test("1 NAND 1 (simple)",
		"local b = bitfield:new(1, 1); b:NAND(1); return tostring(b)", '0b')

test:newsubsection("NOR")
	test("0 NOR 0 (simple)",
		"local b = bitfield:new(0, 1); b:NOR(0); return tostring(b)", '1b')
	test("0 NOR 1 (simple)",
		"local b = bitfield:new(0, 1); b:NOR(1); return tostring(b)", '0b')
	test("1 NOR 0 (simple)",
		"local b = bitfield:new(1, 1); b:NOR(0); return tostring(b)", '0b')
	test("1 NOR 1 (simple)",
		"local b = bitfield:new(1, 1); b:NOR(1); return tostring(b)", '0b')

test:newsubsection("XNOR")
	test("0 XNOR 0 (simple)",
		"local b = bitfield:new(0, 1); b:XNOR(0); return tostring(b)", '1b')
	test("0 XNOR 1 (simple)",
		"local b = bitfield:new(0, 1); b:XNOR(1); return tostring(b)", '0b')
	test("1 XNOR 0 (simple)",
		"local b = bitfield:new(1, 1); b:XNOR(0); return tostring(b)", '0b')
	test("1 XNOR 1 (simple)",
		"local b = bitfield:new(1, 1); b:XNOR(1); return tostring(b)", '1b')

test:newsubsection("shifts")
	test("single shift left (simple)",
		"local b = bitfield:new(1); b:shift(1); return b.value", 2)
	test("multi shift left (simple)",
		"local b = bitfield:new(1); b:shift(4); return b.value", 16)
	test("single shift right (simple)",
		"local b = bitfield:new(2); b:shift(-1); return b.value", 1)
	test("multi shift left (simple)",
		"local b = bitfield:new(16); b:shift(-4); return b.value", 1)

-------------------------------------------------------------------------
test:newsection("FUASSM")
require 'asm'

test:newsubsection("Overview")
	test("FUASSM parsing", 
		"return 'junk'", "useful")
	test("FUASSM encoding",
		"return 'meh'", "good")

test:newsubsection("Build examples")
	test("load 'count10.asm'",
		"local chk = asm.parse(asm.load('examples/count10.asm')) return not not chk", true)
	test("load 'jump.asm'",
		"local chk = asm.parse(asm.load('examples/jump.asm')) return not not chk", true)
	test("load 'show.asm'",
		"local chk = asm.parse(asm.load('examples/show.asm')) return not not chk", true)
	test("load 'test.asm'",
		"local chk = asm.parse(asm.load('examples/test.asm')) return not not chk", true)
	test("load 'mnz-test.asm'",
		"local chk = asm.parse(asm.load('examples/mnz-test.asm')) return not not chk", true)


test:newsection("Machine")

require 'machine'

test:newsubsection("Opcodes")
	test("MNZ, conditional set - true",
		[=[
		local m = machine:new(1)
		testp = {0x05, 0x01, 0x08, 0x05, 0x01, 0xa0, 0x0e, 0x41, 0x2a, 0xff}
		m:load(testp)
		m:cycle(10)
		return m.memory[42]
		]=], 1)
	test("MNZ, conditional set - false",
		[=[
		local m = machine:new(1)
		testp = {0x05, 0x01, 0x08, 0x00, 0x00, 0xa0, 0x0e, 0x41, 0x2a, 0xff}
		m:load(testp)
		m:cycle(10)
		return m.memory[42]
		]=], 1)



-------------------------------------------------------------------------
---- End tests


