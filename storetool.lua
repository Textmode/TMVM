local name, ID, sectors, blocks, bytes = ...

assert(name and ID and sectors and blocks and bytes,
	"must specify a full list of parms, in order: [filename] [ID (4bytes)] [sectors] [blocks] [bytes]")

sectors, blocks, bytes = tonumber(sectors), tonumber(blocks), tonumber(bytes)

-- these are binary-prefix quanities, thus kibi, mebi, gibi, etc.
local bifactor = 1024
local kibibyte = bifactor
local mebibyte = kibibyte * bifactor
local gibibyte = mebibyte * bifactor

-- checks

-- this isn't just a "I hate big files check", many Lua's aren't compiled
--  with large file support. and better it fail here, rather than after 
--  wasting time writting 2GBs
if (sectors*blocks*bytes)+16 > 2*gibibyte then
	print("Unable to create file: Total filesize would be excessive.")
	return 1
end

if sectors > 256 then
	print("Ilegal number of sectors, must be in the range 1..256")
	return 1
end

if blocks > 256 then
	print("Ilegal number of blocks per sector, must be in the range 1..256")
	return 1
end

if bytes > 256 then
	print("Ilegal number of bytes per block, must be in the range 1..256")
	return 1
end

-- we begin.

local magicnumber = string.char(68, 83, 1, 0) -- 'DS' $1 $0 (DataStore 1.0)

-- bytes range from 0..255, legal values are 1..256. so we subtract one
-- and everyone is happy, no?
local hdr = string.char(sectors-1, blocks-1, bytes-1) .. ID

-- now we actually do it.
local f, err = io.open(name, 'wb')

if not f then
	print(string.format("Unable to create file: %s", tostring(err)))
	return 1
end

assert(f, err)

-- magic number + version, length, and header.
f:write(magicnumber, string.char(#hdr), hdr) 

-- prepare a blank sector for writing... (should max at 16KiB)
local blk = string.rep(string.char(0), blocks*bytes)

local t, err
-- write a sector at a time
for i=1, (sectors) do
	t, err = f:write(blk)
	if not t then
		print(string.format("Unable to complete file: %s", err))
	end
end
f:close()

return 0

