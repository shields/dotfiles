--- === WindowClipper ===
---
--- Automatically keeps windows within screen boundaries by adjusting their size and position.
---

local obj = {}
obj.__index = obj

-- Metadata
obj.name = "WindowClipper"
obj.author = "Michael Shields <shields@msrl.com>"
obj.license = "Apache 2.0 - https://www.apache.org/licenses/LICENSE-2.0"

-- Internal state
obj.watcher = nil
obj.logger = nil

--- WindowClipper.logLevel
--- Variable
--- WindowClipper log level. Default: 'debug'
obj.logLevel = 'debug'

--- WindowClipper:init()
--- Method
--- Initialize the spoon
---
--- Parameters:
---  * None
---
--- Returns:
---  * The WindowClipper object
function obj:init()
    self.logger = hs.logger.new('WindowClipper')
    return self
end

--- WindowClipper:setLogLevel(level)
--- Method
--- Sets the log level for the spoon
---
--- Parameters:
---  * level - String with the desired level
---
--- Returns:
---  * The WindowClipper object
function obj:setLogLevel(level)
    if level then
        self.logLevel = level
        self.logger.setLogLevel(level)
    end
    return self
end

--- WindowClipper:start()
--- Method
--- Starts monitoring windows and clipping them when they exceed screen boundaries
---
--- Parameters:
---  * None
---
--- Returns:
---  * The WindowClipper object
function obj:start()
    self.logger.i("Starting WindowClipper")

    if self.watcher then
        self:stop()
    end

    self.watcher = hs.window.filter.new(nil)
    self.watcher:subscribe(hs.window.filter.windowMoved, function(win)
        if win then
            local appName = win:application():name()
            local frame = win:frame()
            local screen = win:screen():frame()
            local changed = false

            self.logger.df("Window moved: %s (%d,%d,%d,%d)", appName, frame.x, frame.y, frame.w, frame.h)
            self.logger.df("Screen bounds: (%d,%d,%d,%d)", screen.x, screen.y, screen.w, screen.h)

            -- Ensure window is within screen bounds
            if frame.x < screen.x then
                local oldX = frame.x
                frame.w = frame.w - (screen.x - frame.x)
                frame.x = screen.x
                changed = true
                self.logger.df("Clipped left edge: x %d -> %d, width %d", oldX, frame.x, frame.w)
            end

            if frame.y < screen.y then
                local oldY = frame.y
                frame.h = frame.h - (screen.y - frame.y)
                frame.y = screen.y
                changed = true
                self.logger.df("Clipped top edge: y %d -> %d, height %d", oldY, frame.y, frame.h)
            end

            if frame.x + frame.w > screen.x + screen.w then
                local oldWidth = frame.w
                frame.w = screen.x + screen.w - frame.x
                changed = true
                self.logger.df("Clipped right edge: width %d -> %d", oldWidth, frame.w)
            end

            if frame.y + frame.h > screen.y + screen.h then
                local oldHeight = frame.h
                frame.h = screen.y + screen.h - frame.y
                changed = true
                self.logger.df("Clipped bottom edge: height %d -> %d", oldHeight, frame.h)
            end

            if changed then
                self.logger.df("Resizing window to: (%d,%d,%d,%d)", frame.x, frame.y, frame.w, frame.h)
                win:setFrame(frame)
            else
                self.logger.df("No clipping needed")
            end
        end
    end)

    return self
end

--- WindowClipper:stop()
--- Method
--- Stops monitoring windows
---
--- Parameters:
---  * None
---
--- Returns:
---  * The WindowClipper object
function obj:stop()
    if self.watcher then
        self.watcher:unsubscribeAll()
        self.watcher = nil
    end
    return self
end

return obj
