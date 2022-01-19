#!/usr/bin/osascript
tell application "System Events"
	tell application process "zoom.us"
		set m to menu 1 of menu bar item "Meeting" of menu bar 1
		if menu item "Mute Audio" of m exists then
			click menu item "Mute Audio" of m
		else if menu item "Unmute Audio" of m exists then
			click menu item "Unmute Audio" of m
		else
			return false
		end if
	end tell
end tell
