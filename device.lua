local bitfield = require "bitfield"

local _M = {_NAME="device", _TYPE="module"}

local _MT = {__index=_M}

-------------------------------------------------------------------------
-- Key values

_M.DEV_TYPE_INVALID   = 'invalid'
_M.DEV_TYPE_NONE      = 'null'
_M.DEV_TYPE_TERMINAL  = 'terminal'
_M.DEV_TYPE_STREAM    = 'stream'
_M.DEV_TYPE_MACHINE   = 'machine'

_M.DEV_STATUS_READY    = 0x00  -- Device is ready
_M.DEV_STATUS_BUSY     = 0x01  -- Device will be available soon, honest.
_M.DEV_STATUS_BLOCKED  = 0x0f  -- Device is blocked and will not become ready without aid
_M.DEV_STATUS_FAULT    = 0xff  -- Device has faulted, and/or died.

local dev_type_names = {
	[_M.DEV_TYPE_NONE]     = "Null Device";
	[_M.DEV_TYPE_TERMINAL] = "Terminal Device";
	[_M.DEV_TYPE_STREAM]   = "Datastream Device";
	[_M.DEV_TYPE_MACHINE]  = "TMVM machine.";
	}

_M.typecount = 5

-------------------------------------------------------------------------
-- Name stuff

local manufacturers = {"Perfetion", "Acme", "Incomputech", "Generic", "Lowest-bid", "ADHOC", "Path-E-Tech"}
	
local names = {
	[_M.DEV_TYPE_NONE]     = {"null port", "data retention dept.", "air gap", "Mu"};
	[_M.DEV_TYPE_TERMINAL] = {"terminal", "display", "client", "PEBKAC"};
	[_M.DEV_TYPE_STREAM]   = {"port", "serial line", "jute twine"};
	[_M.DEV_TYPE_MACHINE]  = {"Machine", "'Titanic'", "'Icarus'", "Analytical Engine", "'Iron'", "'Rex'"};
}

function _M:generatename(devtype)
	if not names[devtype] then return "Unknown Device" end
	local rnd = math.random
	local devnames = names[devtype]
	local manu, devname = manufacturers[rnd(#manufacturers)], devnames[rnd(#devnames)]
	return string.format("%s %s", manu, devname)
end

namer = namedevice

catalogue = {}

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
	
	local err
	
	if d.type == _M.DEV_TYPE_TERMINAL
		or d.type == _M.DEV_TYPE_STREAM then
		d=initstream(d)
	elseif d.type == _M.DEV_TYPE_NONE then
		d=initnull(d)
	elseif catalogue[d.type] then
		d, err = catalogue[d.type].init(d)
	else
		error(("Unknown device type: '%s'"):format(tostring(d.type)))
	end

	return d, err
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
			return pm, "Okay"
		end
	end
	
	return false, "Could not find valid port-mapping"
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
		return _M.DEV_STATUS_READY, "Okay";
	elseif self.type == _M.DEV_TYPE_NONE then

		self.started = true
		return _M.DEV_STATUS_READY, "Okay";
	elseif self.type == _M.DEV_TYPE_CLOCK then
		-- nothing doing.
		self.started = true
		return _M.DEV_STATUS_READY, "Okay";
	elseif catalogue[self.type] then
		return catalogue[self.type].start(self)
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

-- librum
-- with more than a bit of inspiration from package.*

_M.loaded = {}

local devicepath = "devices/?.lua;devices/?.drv;"

function _M:locate(lib)
	assert(type(lib) == 'string', "Not a valid librum name: " .. tostring(lib))
	if _M.loaded[lib] then
		return _M.loaded[lib]
	else
		local s, tmp, err
		for p in devicepath:gmatch("[^;]*") do
			tmp = p:gsub("?", lib)
			tmp, err = loadfile(tmp)
			if tmp then
				s, tmp, err = pcall(tmp)
				if s then
					return tmp, err
				else
					break
				end
			end
		end
		return false, "could not be found"
	end
end

function _M:load(lib)
	local dev, err = _M:locate(lib)
	assert(dev, tostring(err))
	return _M:registerdevice(lib, dev)
end

function  _M:registerdevice(id, d)
	assert(id, "A device type must have an identifier")
	assert(d.regdata, "A device must provide valid registration data")
	
	local rd = d.regdata
	assert(rd.name, "A device type must have a name")
	assert(rd.desc, "Device descriptions are not optional!")
	assert(type(rd.names) == 'table' and rd.names[1], "At least one instance name must be provided!")
	assert(rd.typename, "A typename must be provided")
	
	self[rd.typename] = id
	catalogue[id] = d
	
	return id
end


-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

