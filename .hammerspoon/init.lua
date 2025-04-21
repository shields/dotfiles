package.path = package.path .. ";" .. os.getenv("HOME") .. "/.hammerspoon/Spoons/?.spoon/init.lua"

hs.loadSpoon("EmmyLua")

print("Hammerspoon config loaded")
