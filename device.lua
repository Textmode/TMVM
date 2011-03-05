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
			return 0;
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
				local d  = ((t.hour*60)+(t.min*60)*60)+t.sec
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
		local fname = string.format("%s--%d--%s.log", self.host.name, self.idx, self.name)
		self.stream = io.open(fname, "w")
		assert(self.stream, "Could not open stream")
		
		self.started = true
		return _M.DEV_STATUS_READY;
	elseif self.type == _M.DEV_TYPE_NONE then

		self.started = true
		return _M.DEV_STATUS_READY;
	elseif self.type == _M.DEV_TYPE_CLOCK then
		-- nothing doing.
		self.started = true
		return _M.DEV_STATUS_READY;
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
