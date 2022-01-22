package.loaded.ParseLua = nil
package.loaded.FormatBeautify = nil
local parser = require("ParseLua")
local inspect = require("inspect")
local FormatBeautify = require("FormatBeautiful")
local component = require("component")
local gpu = component.gpu

local function split(str, sep)
    sep = sep or " "
    local t = {}
    local s = 0 -- substring start
    while s do
        local e = str:find(sep, s+1) -- substring end
        t[#t+1] = str:sub(s+1, e and e - 1 or -1)
        s = e
    end
    return t
end

local validLines = {}
local lastValidLine = 0
local hooks = {}
hooks.statement = function(statement, visibleVars)
    local line = statement.FirstLine
    local data = {}
    validLines[line] = true
    lastValidLine = line
    data[#data+1] = string.format("if breakpoints[%d] then __krisDebug(%d,{", line, line)
    for i = 1, #visibleVars do
        data[#data+1] = string.format("%s,", visibleVars[i])
    end
    data[#data+1] = "},{"
    for i = 1, #visibleVars do
        data[#data+1] = string.format("\"%s\",", visibleVars[i])
    end
    data[#data+1] = "}) end"
    return {
        AstType = "VerbatimCode",
        Data = table.concat(data)
    }
end

local function findNextBreakLine(fromLine)
    local i = fromLine + 1
    while not validLines[i] and i <= lastValidLine do
        i = i + 1
    end
    if i <= lastValidLine then
        return i
    end
end

local function printUI()

end

_ENV.breakpoints = {}
_ENV.__krisDebug = function(line, vars, varnames)
    coroutine.yield(line, vars, varnames)
end

local argv = {...}
local f
if argv[1] then
    f = io.open(argv[1])
else
    error("Missing argument: filename")
end
local lines = f:read("*all")
f:close()
local ok, tree = parser(lines, {disableEmitLeadingWhite=true,
                                disableEmitTokenList=true}, hooks)
if not ok then
    error(string.format("Error parsing %s: %s", argv[1], tree))
end
local beautified = FormatBeautify(tree)
local loaded = load(beautified)

local programCoro = coroutine.create(loaded)
local debugCoro = coroutine.create(function()
    local cmd
    local curLine = 0
    while cmd ~= "exit" do
        local input = io.read()
        local subs = split(input, "%s")
        
        cmd = subs[1]
        if cmd == "b" or cmd == "break" then
            local line = findNextBreakLine(tonumber(subs[2]))
            _ENV.breakpoints[line] = true
            print(string.format("Setting breakpoint at line %d", line))
        elseif cmd == "s" or cmd == "step" then
            --local count = subs[2] and tonumber(subs[2]) or 1
            local line = findNextBreakLine(curLine)
            _ENV.breakpoints[line] = true
            _ENV.breakpoints[curLine] = false
            print(string.format("Setting breakpoint at line %d", line))
            coroutine.yield()
        elseif cmd == "c" or cmd == "continue" then
            _ENV.breakpoints[curLine] = false
            coroutine.yield()
        end
    end
end)

while true do
    local ok, err = coroutine.resume(debugCoro)
    if not ok then
        print(err)
    end
    if coroutine.status(debugCoro) == "dead" then break end
    print("resuming program")
    local ok, line, vars, varnames = coroutine.resume(programCoro)
    if not ok then break end
end
    