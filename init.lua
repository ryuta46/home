
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

local function warn(message)
    hs.alert.show(message)
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


hs.window.filter.new()
    :subscribe(hs.window.filter.windowFocused,function() 
        local focused = hs.window.focusedWindow()
        if focused:application():name() == 'Xcode' then
            -- warn("Focused XCode")
            enableAll(xcodeBindings) 
        else
            -- warn("Unfocused XCode")
            disableAll(xcodeBindings)
            markMode:disable()
            commandMode:disable()
        end
        if focused:application():name() == 'TradingView' then
            enableAll(tradingViewBindings) 
        else
            disableAll(tradingViewBindings)
            markMode:disable()
            commandMode:disable()
        end
        
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

hs.window.filter.new({'Skype', 'Discord'})
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


hs.hotkey.bind({"option"}, "f", function()
    local app = hs.appfinder.appFromName("Finder")
    if app ~= nil and app:isFrontmost() then
        app:hide()
    else 
        hs.application.launchOrFocus("Finder")
    end
end)


function getTargetText()
    local elem = hs.uielement.focusedElement()
    if elem then
        local sel = elem:selectedText()
        if (sel == nil or sel == "") then
            sel = hs.pasteboard.getContents()
            if (sel == nil or sel == "") then
                return
            end
        end
        return sel
    else
        return
    end
end


--
-- to show hex of selected text.
--
function showHexDescription()
    local targetText = getTargetText()
    if targetText == nil then
        return
    end
    local targetNum = tonumber(targetText)
    if targetNum == nil then
        return
    end
    message = string.format("Base10: %d\nBase16: 0x%X", targetNum, targetNum)

    local showSeconds = 5
    hs.alert.show(message, showSeconds)
end

hs.hotkey.bind({'shift', 'ctrl'}, 'h', function() showHexDescription() end)

--
-- to show date of selected timestamp.
--
function showTimeDescription()
    local targetText = getTargetText()
    if targetText == nil then
        return
    end
    local targetNum = tonumber(targetText)
    if targetNum == nil then
        return
    end
    local formattedTime
    if targetNum > 10000000000 then
        formattedTime = os.date("%Y-%m-%d %H:%M:%S", math.floor(targetNum / 1000))
        local millis = targetNum % 1000
        formattedTime = string.format("%s.%03d", formattedTime, millis)
    else
        formattedTime = os.date("%Y-%m-%d %H:%M:%S", targetNum)
    end
    
    message = string.format("Timestamp: %d\nDate: %s", targetNum, formattedTime)

    local showSeconds = 5
    hs.alert.show(message, showSeconds)
end


hs.hotkey.bind({'shift', 'ctrl'}, 't', function() showTimeDescription() end)


displayUpdate = hs.eventtap.new( {hs.eventtap.event.types.keyDown},
    function(e)
        if hs.keycodes.map[e:getKeyCode()] == 'f2' and isEqualTable(e:getFlags(), {cmd = true, fn = true}) then
            local event = hs.eventtap.event.newSystemKeyEvent("BRIGHTNESS_UP", true)
            event:setFlags({cmd = true})
            event:post()
        end
    end)

displayUpdate:start()


-- for trading view
tradingViewBindings = {
    -- mark mode
    hs.hotkey.new({'ctrl'}, 'space', function() markMode:enable() end),
    -- command mode
    hs.hotkey.new({'ctrl'}, 'x', function() commandMode:enable() end),

    -- move to up/down of line. for popup window
    createKeyRemap({'ctrl'}, 'p', {}, 'up'),
    createKeyRemap({'ctrl'}, 'n', {}, 'down'),

    -- undo
    -- createKeyRemap({'ctrl'}, '/', {'cmd'}, 'z'),

    -- search
    -- createKeyRemap({'ctrl'}, 's', {'cmd'}, 'f'),

    -- paste
    createKeyRemap({'ctrl'}, 'y', {'cmd'}, 'v'),

    -- kill line
    hs.hotkey.new({'ctrl'}, 'k', function()
        markMode:enable()
        pressKey({'ctrl'}, 'e')
        pressKey({'ctrl'}, 'w')
    end),
}


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
