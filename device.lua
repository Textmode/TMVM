local bitfield = require "bitfield"

local _M = {_NAME="device"}

local _MT = {__index=_M}

-------------------------------------------------------------------------
-- Key values

_M.DEV_TYPE_NONE      = 0x00
_M.DEV_TYPE_TERMINAL  = 0x01
_M.DEV_TYPE_STREAM    = 0x02
_M.DEV_TYPE_STORE     = 0x03
_M.DEV_TYPE_NETWORK   = 0x04
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

-------------------------------------------------------------------------
-- exports

-- Creates a new device based on the given parms
--
-- returns: device
function _M:new(t)
	assert(t.type, "Cannot create devices with no type!")
	local d = {type=t.type, name=t.name or _M:generatename(t.type), portmaps = t.portmaps}
	setmetatable(d, _MT)
	
	if d.type == DEV_TERMINAL or devtype == DEV_STREAM then
		local dev =  {idx=idx, type=devtype}
		
		self.receive = function(self, pin, val)
			self.stream:write(string.char(val))
			return 0
		end
		
		self.send = function(self, pin)
			return 0;
		end
		d.portmaps = {{1}}
		
	elseif devtype == DEV_NONE then
		self.receive = function(self, pin, val)
			return 0
		end
		
		self.send = function(self, pin)
			return 0
		end

		d.portmaps = {{0}}

	end
	
	return d
end

-- examines the given portmap and returns a non-conflicting portmap
--
-- returns: portmap
--   or
-- returns: false, errmsg
function _M.findports(dev, portmap)
	
	local p_map, port
	for i=1,#dev.portmaps do
		
		p_map = dev.portmaps[i]
		for j=1, #p_map do
			port = p_map[j]
			if portmap[port] then
				p_map = nil
				break
			end
		end
		if p_map then break end; -- we found one, hurrah!
	end

	if not p_map then
		return false, "Could not find valid portmapping"
	else	
		return p_map
	end
	
	return false, "Unknown error"
end

-- Starts the device, and readies it for use
--
-- returns status
--   or
-- returns false, errmsg
function _M:start(host, idx)
	self.host = host

	if self.type == DEV_TYPE_TERMINAL or self.type == DEV_TYPE_STREAM then
		local fname = string.format("%s--%d--%s.log", self.host.name, self.idx, self.name)
		self.stream = io.open(fname, "w")
		
		return _M.DEV_STATUS_READY;
	elseif devtype == DEV_NONE then

		return _M.DEV_STATUS_READY;
	end
	
	return false, "Unknown error"
end

function _M:registerport(port, address)
	self.portmap[address]=port
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

-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

