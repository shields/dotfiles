package.path = package.path .. ";" .. os.getenv("HOME") .. "/.hammerspoon/Spoons/?.spoon/init.lua"

hs.loadSpoon("EmmyLua")
hs.loadSpoon("WindowClipper")
    :setLogLevel('debug')
    :start()

hs.logger.new('Hammerspoon', 'info'):i("Hammerspoon config loaded")