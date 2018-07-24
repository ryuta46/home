
local VK_JIS_YEN = 0x5d
local VK_JIS_UNDERSCORE = 0x5e

local VK_JIS_LEFT_BRACKET = 0x1E
local VK_JIS_RIGHT_BRACKET = 0x2A

local VK_LEFT_COMMAND = 0x37
local VK_RIGHT_COMMAND = 0x36
local VK_EISUU = 0x66
local VK_KANA = 0x68

local showInfo = false
local function info(message)
    if showInfo then
        hs.alert.show(message)
    end
end
hs.hotkey.bind({'cmd', 'shift', 'ctrl'}, 'D', function() showInfo = not(showInfo) end)
--
-- to switch eisuu/kana with single command press.
--
switchInputMethodPrevKey = 0xFF

-- invalidate previous key
switchInputMethodInvalidate = hs.eventtap.new({hs.eventtap.event.types.keyDown}, 
    function(e) 
        switchInputMethodPrevKey = 0xFF 
    end
)
switchInputMethodInvalidate:start()

repeatedCountOfEisuu = 0
reverseEisuuKana = false

switchInputMethod = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, 
    function(e) 
        local keyCode = e:getKeyCode()
        local isCmd = e:getFlags()['cmd']
        info("flagsChanged code:"..tostring(keyCode))
        info("flagsChanged flags:"..tostring(isCmd))

        local eisuuTrigger = VK_LEFT_COMMAND
        local kanaTrigger = VK_RIGHT_COMMAND
        info("is reverse"..tostring(reverseEisuuKana))
        if reverseEisuuKana then
            eisuuTrigger = VK_RIGHT_COMMAND
            kanaTrigger = VK_LEFT_COMMAND
        end
        if isCmd then
            switchInputMethodPrevKey = keyCode
        else
            if switchInputMethodPrevKey == eisuuTrigger then
                hs.eventtap.keyStroke({}, VK_EISUU)
                repeatedCountOfEisuu = 0
            elseif switchInputMethodPrevKey == kanaTrigger then
                hs.eventtap.keyStroke({}, VK_KANA)
                repeatedCountOfEisuu = repeatedCountOfEisuu + 1
                if repeatedCountOfEisuu >= 5 then
                    repeatedCountOfEisuu = 0
                    reverseEisuuKana = not reverseEisuuKana
                end

            end
        end
    end
)
switchInputMethod:start()


--
-- to emacs like keybindins in Xcode
--
local function getTableLength(t)
    local count = 0
    for _ in pairs(t) do count = count + 1 end
    return count
end

local function isEqualTable(t1, t2)
    if getTableLength(t1) ~= getTableLength(t2) then return false end
    for k, v in pairs(t1) do
        if t2[k] ~= v then
            return false
        end
    end
    return true
end

local function disableAll(keySet)
    for k, v in pairs(keySet) do v:disable() end
end

local function enableAll(keySet)
    for k, v in pairs(keySet) do v:enable() end
end

local function pressKey(modifiers, key)
    modifiers = modifiers or {}
    hs.eventtap.keyStroke(modifiers, key, 20 * 1000)
end

local function pressKeyFunc(modifiers, key)
    return function() pressKey(modifiers, key) end
end

local function createKeyRemap(srcModifiers, srcKey, dstModifiers, dstKey)
    dstFunc = function() pressKey(dstModifiers, dstKey) end
    return hs.hotkey.new(srcModifiers, srcKey, dstFunc, nil, dstFunc)
end

SubMode = {}
SubMode.new = function(name, commandTable, othersFunc)
    local obj = {}
    obj.name = name
    obj.commandTable = commandTable
    obj.othersFunc = othersFunc
    obj.commandWatcher = {}
    obj.enable = function(self)
        info(self.name.." start")
        self.commandWatcher:start()
    end

    obj.disable = function(self)
        info(self.name.." end")
        self.commandWatcher:stop()
    end

    obj.commandWatcher = hs.eventtap.new( {hs.eventtap.event.types.keyDown},
        function(tapEvent)
            for k,v in pairs(obj.commandTable) do
                if v.key == hs.keycodes.map[tapEvent:getKeyCode()] and isEqualTable(v.modifiers, tapEvent:getFlags()) then
                    info(obj.name.." end")
                    obj.commandWatcher:stop()
                    if v.func then
                        v.func()
                    end
                    return true
                end
            end

            if obj.othersFunc then
                return othersFunc(tapEvent)
            end
        end)
    return obj
end


markMode = SubMode.new(
    "Mark Mode",
    {
        {modifiers = {ctrl = true}, key = 'space'}, -- only disables mark mode
        {modifiers = {ctrl = true}, key = 'g'}, -- only disables mark mode
        {modifiers = {ctrl = true}, key = 'w', func = pressKeyFunc({'cmd'}, 'x')},
        {modifiers = {alt = true},  key = 'w', func = pressKeyFunc({'cmd'}, 'c')}
    },

    function(tapEvent) -- force shift on
        flags = tapEvent:getFlags()
        flags.shift = true
        tapEvent:setFlags(flags)
        return false
    end
)

commandMode = SubMode.new(
    "Command Mode",
    {
        {modifiers = {ctrl = true}, key = 'g'}, -- only disables command mode
        {modifiers = {ctrl = true}, key = 'f', func = pressKeyFunc({'cmd', 'shift'}, 'o')},
        {modifiers = {ctrl = true}, key = 's', func = pressKeyFunc({'cmd'}, 's')},
        -- next tab
        {modifiers = {},            key = 'n', func = pressKeyFunc({'cmd', 'shift'}, VK_JIS_RIGHT_BRACKET)},
        -- previous tab
        {modifiers = {},            key = 'p', func = pressKeyFunc({'cmd', 'shift'}, VK_JIS_LEFT_BRACKET)},
        -- close tab
        {modifiers = {},            key = 'k', func = pressKeyFunc({'cmd'}, 'w')},
    }
)

xcodeBindings = {
    -- mark mode
    hs.hotkey.new({'ctrl'}, 'space', function() markMode:enable() end),
    -- command mode
    hs.hotkey.new({'ctrl'}, 'x', function() commandMode:enable() end),

    -- etc
    -- jump to beginning/end of document
    createKeyRemap({'alt', 'shift'}, ',', {'cmd'}, 'up'),
    createKeyRemap({'alt', 'shift'}, '.', {'cmd'}, 'down'),

    -- move to up/down of line. for popup window
    createKeyRemap({'ctrl'}, 'p', {}, 'up'),
    createKeyRemap({'ctrl'}, 'n', {}, 'down'),

    -- undo
    createKeyRemap({'ctrl'}, '/', {'cmd'}, 'z'),

    -- search
    createKeyRemap({'ctrl'}, 's', {'cmd'}, 'f'),

    -- paste
    createKeyRemap({'ctrl'}, 'y', {'cmd'}, 'v'),

    -- kill line
    hs.hotkey.new({'ctrl'}, 'k', function()
        markMode:enable()
        pressKey({'ctrl'}, 'e')
        pressKey({'ctrl'}, 'w')
    end),
}

hs.window.filter.new('Xcode')
    :subscribe(hs.window.filter.windowFocused,function() enableAll(xcodeBindings) end)
    :subscribe(hs.window.filter.windowUnfocused,function()
        disableAll(xcodeBindings)
        markMode:disable()
        commandMode:disable()
    end)

--
-- to integrate message send with Cmd + Enter in messenger apps.
--
slackSendBindings = hs.eventtap.new( {hs.eventtap.event.types.keyDown},
    function(e)
        if hs.keycodes.map[e:getKeyCode()] == 'return' and isEqualTable(e:getFlags(), {cmd = true}) then
            info("Cmd return -> return")
            e:setFlags({cmd = false})
        elseif hs.keycodes.map[e:getKeyCode()] == 'return' and isEqualTable(e:getFlags(), {}) then
            info("return -> shift + return")
            e:setFlags({shift = true})
        end
    end)

hs.window.filter.new({'Slack', 'Skype', 'Discord'})
    :subscribe(hs.window.filter.windowFocused,function() slackSendBindings:start() end)
    :subscribe(hs.window.filter.windowUnfocused,function() slackSendBindings:stop() end)

--
-- to input backslash in JetBrains IDE.
--

function flagsMatches(flags, modifiers)
    local set = {}
    for _, k in ipairs(modifiers) do set[string.lower(k)] = true end
    for _, k in ipairs({'fn', 'cmd', 'ctrl', 'alt', 'shift'}) do
        if set[k] ~= flags[k] then return false end
    end
    return true
end

-- NEVER define as local variable!
jisKeyboardFilter = hs.eventtap.new({
    hs.eventtap.event.types.keyDown,
    hs.eventtap.event.types.keyUp
}, function(event)
    local c = event:getKeyCode()
    local f = event:getFlags()
    -- log.d(...)
    if c == VK_JIS_YEN then
        -- To input \ even if JVM, toggle Option key status when Yen key.
        if flagsMatches(f, {'alt'}) then
            event:setFlags({})
        elseif flagsMatches(f, {}) then
            event:setFlags({alt=true})
        end
    elseif c == VK_JIS_UNDERSCORE then
        -- Also map single undetscore (_) key to backslash (\).
        if flagsMatches(f, {}) then
            event:setKeyCode(VK_JIS_YEN)
            event:setFlags({alt=true})
        end
    end
end)
jisKeyboardFilter:start()

--
-- for debug
--
local function showKeyPress(tapEvent)
    local code = tapEvent:getKeyCode()
    local charactor = hs.keycodes.map[tapEvent:getKeyCode()]
    hs.alert.show(tostring(code)..":"..charactor, 1.5)
end

local keyTap = hs.eventtap.new( {hs.eventtap.event.types.keyDown}, showKeyPress)

k = hs.hotkey.modal.new({"cmd", "shift", "ctrl"}, 'P')

function k:entered()
    hs.alert.show("Enabling Keypress Show Mode", 1.5)
    keyTap:start()
end

function k:exited()
    hs.alert.show("Disabling Keypress Show Mode", 1.5)
end

k:bind({"cmd", "shift", "ctrl"}, 'P', function()
    keyTap:stop()
    k:exit()
end)
