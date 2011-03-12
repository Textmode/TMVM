local device = require'device'

local _M = {_NAME="store", _TYPE="module"}

local _MT = {__index=_M}

_M.regdata = {
		name="Persistant Storage Device";
		desc="Allows data to be stored persistantly accross reboots or crashes.";
		names = {"storage", "floppy drive", "printer/scanner assembly", "magnet and steady hand"};
		typename="DEV_TYPE_STORE";	
		};


function _M.init(d)
	d.portmaps = {{16, 17, 18, 19},
	              {32, 33, 34, 35},
	              {64, 65, 66, 67}
	             }
	
	d.sector      = 0; d.block       = 0; d.byte        = 0
	d.maxsectors  = 0; d.maxblocks   = 0; d.maxbytes    = 0
	
	d.imagename = string.format("%s.store", "shared")
	
	d.receive = receive
	
	d.send = send
	
	return d, "Okay"

end

function _M:start()
	local img, err = io.open(self.imagename, 'r+b')
	if img then
		self.image = img
		
		if img:read(2) ~= "DS" then
			-- not a DataStore file.
			img:close()
			return false, "Not a valid store"
		end
		local version = string.format("v%d.%d", img:read(1):byte(), img:read(1):byte())
		if  version ~= "v1.0" then
			-- unknown version
			img:close()
			return false, "Unknown version"
		end
		
		local len = img:read(1):byte()
		local hdr = img:read(len) -- load the header (up to 255 in size)
		      hdr = {hdr:byte(1, -1)}
		
		
		-- the stored value is 0..255, but we support 1..256, so +1
		self.maxsectors  = hdr[1]+1
		self.maxblocks   = hdr[2]+1
		self.maxbytes    = hdr[3]+1
		self.modelID     = string.char(hdr[4], hdr[5], hdr[6], hdr[7])
		self.header = hdr
		self.offset = 5+len -- magic number, plus the length of the header
		
		local c, s, n = img:seek(), img:seek('end'), img:seek('set', self.offset)
		
		assert(c == n, "Not returning to the right position. logic error!")
		
		if s < len+(self.maxsectors*self.maxblocks*self.maxbytes) then
			-- truncated file
			img:close()
			return false, "Truncated file"
		end
		
		self.started = true
		return device.DEV_STATUS_READY, "Okay"
	else -- not img
		return false, ("Could not open image '%s': %s"):format(fname, tostring(err))
	end
	
	return false, "unknown store start error"
end


local function receive (self, pin, val)
	assert(self.started, "Not Started!")
	if pin == 1 then
		self.sector = val
	elseif pin == 2 then
		self.block = val
	elseif pin == 3 then
		self.byte = val
	elseif pin == 4 then
		local pos = (self.sector*(self.maxblocks*self.maxbytes) +
		            (self.block*self.maxbytes)) +
		            (self.byte)
		self.image:seek('set', self.offset + pos)
		self.image:write(string.char(val))
	end
	return 0
end
	
local function send(self, pin)
	assert(self.started, "Not Started!")
	if pin == 1 then
		return self.sector
	elseif pin == 2 then
		return self.block
	elseif pin == 3 then
		return self.byte
	elseif pin == 4 then
		local pos = (self.blocks*(self.maxblocks*self.maxbytes) +
		            (self.block*self.maxbytes)) +
		            (self.byte)
		self.image:seek('set', self.offset + pos)
		return self.image:read(1)
	end
	return 0
end


return _M

