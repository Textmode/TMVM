local device = require'device'

local _M = {_NAME="clock", _TYPE="module"}

local _MT = {__index=_M}

_M.regdata = {
		name="Real-Time Clock";
		desc="Provides information on the real-world time";
		names = {"clock", "hourglass", "sundial", "temporal guesstimator"};
		typename="DEV_TYPE_CLOCK";	
		};


function _M.init(d)
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


-- MODULE TAIL
-------------------------------------------------------------------------

-- in 5.1.x the custom is to set a global equal to the module name, in all others this ill-behaved.
if _VERSION == "Lua 5.1" then _G[_M._NAME]=_M end

return _M

