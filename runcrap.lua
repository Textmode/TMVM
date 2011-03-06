require 'machine'
require'asm'

m=machine:new()

local file = arg[1]

assert(file, "what the? I've got nothing here!")

if file:match(".*.asm") then
	chk= asm.load(file, true)
	chk=asm.parse(chk)
	m:load(chk)
elseif file:match(".*.crap") then
	local file, err = io.open(file, "rb")
	assert(file, err)
	chk = file:read("*a")
	m:load(chk)
end

m:run()

