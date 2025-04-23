--- === UTCMenuBar ===
---
--- Displays the current UTC time in the menubar.
---

local obj = {}
obj.__index = obj

-- Metadata
obj.name = "UTCMenuBar"
obj.author = "Michael Shields <shields@msrl.com>"
obj.license = "Apache 2.0 - https://www.apache.org/licenses/LICENSE-2.0"

-- Internal state
obj.timer = nil
obj.menubar = nil
obj.logger = nil

--- UTCMenuBar.logLevel
--- Variable
--- UTCMenuBar log level. Default: 'info'
obj.logLevel = 'info'

--- UTCMenuBar.updateInterval
--- Variable
--- Number of seconds between updates. Default: 60
obj.updateInterval = 60

--- UTCMenuBar:init()
--- Method
--- Initialize the spoon
---
--- Parameters:
---  * None
---
--- Returns:
---  * The UTCMenuBar object
function obj:init()
    self.logger = hs.logger.new('UTCMenuBar')
    self.menubar = hs.menubar.new()
    return self
end

--- UTCMenuBar:setLogLevel(level)
--- Method
--- Sets the log level for the spoon
---
--- Parameters:
---  * level - String with the desired level
---
--- Returns:
---  * The UTCMenuBar object
function obj:setLogLevel(level)
    if level then
        self.logLevel = level
        self.logger.setLogLevel(level)
    end
    return self
end

--- UTCMenuBar:setUpdateInterval(seconds)
--- Method
--- Sets how often the UTC time is updated
---
--- Parameters:
---  * seconds - Update interval in seconds
---
--- Returns:
---  * The UTCMenuBar object
function obj:setUpdateInterval(seconds)
    if seconds then
        self.updateInterval = seconds
    end
    return self
end

--- UTCMenuBar:updateTime()
--- Method
--- Updates the displayed time in the menubar
---
--- Parameters:
---  * None
---
--- Returns:
---  * None
function obj:updateTime()
    local time = os.date("!%H:%M UTC")
    self.menubar:setTitle(time)
    self.logger.d("Updated time: " .. time)
end

--- UTCMenuBar:start()
--- Method
--- Displays the UTC time in the menubar and starts the timer to update it
---
--- Parameters:
---  * None
---
--- Returns:
---  * The UTCMenuBar object
function obj:start()
    self.logger.i("Starting UTCMenuBar")
    
    if not self.menubar then
        self.menubar = hs.menubar.new()
    end
    
    -- Initial update
    self:updateTime()
    
    -- Start timer for updates
    if self.timer then
        self.timer:stop()
    end
    
    -- Calculate seconds until the next minute
    local now = os.time()
    local seconds = 60 - (now % 60)
    
    -- Create timer that fires at the top of each minute
    self.timer = hs.timer.doAfter(seconds, function()
        self:updateTime()
        -- Then set up a repeating timer aligned with minute boundaries
        self.timer = hs.timer.new(self.updateInterval, function() self:updateTime() end)
        self.timer:start()
    end)
    
    return self
end

--- UTCMenuBar:stop()
--- Method
--- Stops the timer and removes the menubar item
---
--- Parameters:
---  * None
---
--- Returns:
---  * The UTCMenuBar object
function obj:stop()
    self.logger.i("Stopping UTCMenuBar")
    
    if self.timer then
        self.timer:stop()
        self.timer = nil
    end
    
    if self.menubar then
        self.menubar:delete()
        self.menubar = nil
    end
    
    return self
end

return obj