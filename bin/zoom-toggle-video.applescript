#!/usr/bin/osascript
tell application "System Events"
	tell application process "zoom.us"
		set m to menu 1 of menu bar item "Meeting" of menu bar 1
		if menu item "Start Video" of m exists then
			click menu item "Start Video" of m
		else if menu item "Stop Video" of m exists then
			click menu item "Stop Video" of m
		else
			return false
		end if
	end tell
end tell
