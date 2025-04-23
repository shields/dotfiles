package.path = package.path .. ";" .. os.getenv("HOME") .. "/.hammerspoon/Spoons/?.spoon/init.lua"

hs.loadSpoon("EmmyLua")
hs.loadSpoon("WindowClipper")
    :setLogLevel('debug')
    :start()

hs.loadSpoon("UTCMenuBar")
    :setLogLevel('info')
    :start()

hs.logger.new('Hammerspoon', 'info'):i("Hammerspoon config loaded")