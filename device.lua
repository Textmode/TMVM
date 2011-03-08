local bitfield = require "bitfield"

local _M = {_NAME="device", _TYPE="module"}

local _MT = {__index=_M}

-------------------------------------------------------------------------
-- Key values

_M.DEV_TYPE_INVALID   = 0x00
_M.DEV_TYPE_NONE      = 0x01
_M.DEV_TYPE_TERMINAL  = 0x02
_M.DEV_TYPE_STREAM    = 0x03
_M.DEV_TYPE_STORE     = 0x04
_M.DEV_TYPE_NETWORK   = 0x05
_M.DEV_TYPE_CLOCK     = 0x06
_M.DEV_TYPE_MACHINE   = 0xff

_M.DEV_STATUS_READY    = 0x00  -- Device is ready
_M.DEV_STATUS_BUSY     = 0x01  -- Device will be availible soon, honest.
_M.DEV_STATUS_BLOCKED  = 0x0f  -- Device is blocked and will not become ready without aid
_M.DEV_STATUS_FAULT    = 0xff  -- Device has faulted, and/or died.

local dev_type_names = {
	[_M.DEV_TYPE_NONE]     = "Null Device";
	[_M.DEV_TYPE_TERMINAL] = "Terminal Device";
	[_M.DEV_TYPE_STREAM]   = "Datastream Device";
	[_M.DEV_TYPE_STORE]    = "Persistant Storage Device";
	[_M.DEV_TYPE_NETWORK]  = "Networking Device";
	[_M.DEV_TYPE_MACHINE]  = "TMVM machine.";
	}


-------------------------------------------------------------------------
-- Name stuff

local manufacturers = {"Perfetion", "Acme", "Incomputech", "Generic", "Lowest-bid", "ADHOC", "Path-E-Tech"}
	
local names = {
	[_M.DEV_TYPE_NONE]     = {"null port", "data retention dept.", "air gap", "Mu"};
	[_M.DEV_TYPE_TERMINAL] = {"terminal", "display", "client", "PEBKAC"};
	[_M.DEV_TYPE_STREAM]   = {"port", "serial line", "jute twine"};
	[_M.DEV_TYPE_STORE]   = {"storage", "floppy drive", "printer/scanner assembly", "magnet and steady hand"};
	[_M.DEV_TYPE_NETWORK]   = {"network", "1bps modem", "foot-net"};
	[_M.DEV_TYPE_MACHINE]  = {"Machine", "'Titanic'", "Analytical Engine", "'Iron'", "'Rex'"};
}

function _M:generatename(devtype)
	if not names[devtype] then return "Unknown Device" end
	local rnd = math.random
	local devnames = names[devtype]
	local manu, devname = manufacturers[rnd(#manufacturers)], devnames[rnd(#devnames)]
	return string.format("%s %s", manu, devname)
end

namer = namedevice


local function initstream(d)
		local dev =  {idx=idx, type=devtype}
		d.stream = {}
		
		d.receive = function(self, pin, val)
			assert(self.started, "Not Started!")
			self.stream:write(string.char(val))
			return 0
		end
		
		d.send = function(self, pin)
			assert(self.started, "Not Started!")
			local val = self.istream:read(1)
			if val then
				return string.byte(val);
			else
				return false, DEV_STATUS_BLOCKED
			end
		end
		if d.type == _M.DEV_TYPE_TERMINAL then
			d.portmaps = {{1}}
		else
			d.portmaps = {{10}, {20}, {30}, {40}}
		end
		
	return d
end

local function initnull(d)
	d.portmaps = {{0}}

	d.receive = function(self, pin, val)
		assert(self.started, "Not Started!")
		return 0
	end
	
	d.send = function(self, pin)
		assert(self.started, "Not Started!")
		return 0
	end
	
	return d
end

local function initstore(d)
	d.portmaps = {{16, 17, 18, 19},{32, 33, 34, 35},{64, 65, 66, 67}}
	
	d.sector      = 0
	d.block       = 0
	d.byte        = 0
	d.maxsectors  = 0
	d.maxblocks   = 0
	d.maxbytes    = 0
	
	d.imagename = string.format("%s.store", "shared")
	
	d.receive = function(self, pin, val)
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
			print('wseek', pos)
		end
		return 0
	end
	
	d.send = function(self, pin)
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
			print('rseek', pos)
			return self.image:read(1)
		end
		return 0
	end
	
	return d

end

local function initclock(d)
	d.portmaps = {{42}}
	
	d.states ={
		CLOCK_STATE_DEFAULT  = 0x0, CLOCK_STATE_SECONDS = 0x1,
		CLOCK_STATE_MINUTES  = 0x2, CLOCK_STATE_HOURS   = 0x3,
		CLOCK_STATE_MONTHDAY = 0x4, CLOCK_STATE_WEEKDAY = 0x4,
		CLOCK_STATE_MONTH    = 0x5, CLOCK_STATE_YEAR    = 0x6,
		CLOCK_STATE_DAYRATIO = 0x7 }
		
	d.state = d.states.CLOCK_STATE_DEFAULT
	
	d.receive = function(self, pin, val)
		assert(self.started, "Not Started!")
		if val >= self.states.CLOCK_STATE_DEFAULT and
				val <= self.states.CLOCK_STATE_YEAR then
				
			self.state = val
		else
			return false, _M.DEV_STATUS_BLOCKED
		end
		return 0
	end
	
	d.send = function(self, pin)
		assert(self.started, "Not Started!")
		local val = self.state
		local st = self.states
		if val >= self.states.CLOCK_STATE_DEFAULT and
				val <= self.states.CLOCK_STATE_YEAR then
			
			if val == st.CLOCK_STATE_DEFAULT then
				return math.floor(os.time() % 256)
			elseif val == st.CLOCK_STATE_SECONDS then
				local t = os.date("!*t")
				return math.floor(t.sec % 256)
			elseif val == st.CLOCK_STATE_MINUTES then
				local t = os.date("!*t")
				return math.floor(t.min % 256)
			elseif val == st.CLOCK_STATE_HOURS then
				local t = os.date("!*t")
				return math.floor(t.hour % 256)
			elseif val == st.CLOCK_STATE_MONTHDAY then
				local t = os.date("!*t")
				return math.floor(t.day % 256)
			elseif val == st.CLOCK_STATE_WEEKDAY then
				local t = os.date("!*t")
				return math.floor(t.wday % 256)
			elseif val == st.CLOCK_STATE_MONTH then
				local t = os.date("!*t")
				return math.floor(t.month % 256)
			elseif val == st.CLOCK_STATE_YEAR then
				local t = os.date("!*t")
				return math.floor((t.year-1980) % 256)
			elseif val == st.CLOCK_STATE_DAYRATIO then
				local t  = os.date("!*t")
				local d  = (((t.hour*60)+(t.min))*60)+t.sec
				local fd = 86400
				return math.floor(((d/fd)*255) % 256)
			end
		else
			return false, _M.DEV_STATUS_BLOCKED
		end
		return 42
	end

	return d
end

-------------------------------------------------------------------------
-- exports

-- Creates a new device based on the given parms
--
-- returns: device
function _M:new(t)
	assert(t.type, "Cannot create devices with no type!")
	local d = {type=t.type,_TYPE='device',  name=t.name or _M:generatename(t.type);
		portmaps = t.portmaps, portmap={}}
	setmetatable(d, _MT)
	
	if d.type == _M.DEV_TYPE_TERMINAL
		or d.type == _M.DEV_TYPE_STREAM then
		d=initstream(d)
	elseif d.type == _M.DEV_TYPE_NONE then
		d=initnull(d)
	elseif d.type == _M.DEV_TYPE_CLOCK then
		d=initclock(d)
	elseif d.type == _M.DEV_TYPE_STORE then
		d=initstore(d)
	else
		error(("Unknown device type: '%s'"):format(tostring(d.type)))
	end

	return d
end

local function testmap(sysmap, testmap)
	local _, p
	for _, p in pairs(testmap) do
		if sysmap[p] then return false end
	end
	
	return true
end

-- examines the given portmap and returns a non-conflicting portmap
--
-- returns: portmap
--   or
-- returns: false, errmsg
function _M.findports(dev, portmap)
	assert(not dev.started, "Can't remap started devices")
	assert(dev and dev.portmaps and portmap, "Findports requires valid parms")

	local _, pm
	for _, pm in pairs(dev.portmaps) do
		if testmap(portmap, pm) then
			return pm
		end
	end
	
	return false, "Could not find valid portmapping"
end

-- Starts the device, and readies it for use
--
-- returns status
--   or
-- returns false, errmsg
function _M:start(host, idx)
	self.host = host
	self.idx = idx
	self.portmaps = nil --note plural

	if self.type == _M.DEV_TYPE_TERMINAL or self.type == _M.DEV_TYPE_STREAM then
		local foname = string.format("%s--%d--%s.log", self.host.name, self.idx, self.name)
		local finame = string.format("%s-%02x.input", dev_type_names[self.type], self.idx)
		local err
		self.stream, err = io.open(foname, "wb")
		assert(self.stream, "Could not open stream: "..tostring(err))
		self.istream, err = io.open(finame, "rb")
		-- failing to open the input is not an error
		
		self.started = true
		return _M.DEV_STATUS_READY;
	elseif self.type == _M.DEV_TYPE_NONE then

		self.started = true
		return _M.DEV_STATUS_READY;
	elseif self.type == _M.DEV_TYPE_CLOCK then
		-- nothing doing.
		self.started = true
		return _M.DEV_STATUS_READY;
	elseif self.type == _M.DEV_TYPE_STORE then
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
			return _M.DEV_STATUS_READY;
		else
			assert(img, ("Could not open image '%s': %s"):format(fname, tostring(err)))
		end
	else
		return false, "Unknown device type: "..tostring(self.type)
	end
	
	return false, "Unknown start error"
end

function _M:registerport(adr, port)
	self.portmap[adr]=port
	return self
end

function _M:writeport(adr, data)
	assert(self.portmap[adr], "Data written to unknown port!")
	if self.receive then
		return self:receive(self.portmap[adr], data)
	end
	return false, _M.DEV_STATUS_BLOCKED
end

function _M:readport(adr, data)
	assert(self.portmap[adr], "Data read from unknown port!")
	if self.send then
		return self:send(self.portmap[adr])
	end
	return false, _M.DEV_STATUS_BLOCKED
end

function _MT.__tostring(self)
	return string.format('%s, "%s"', dev_type_names[self.type], self.name)
end

-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

