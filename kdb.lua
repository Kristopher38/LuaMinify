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

local function strip(str)
    return (string.gsub(string.gsub(str, "^%s+", ""), "%s+$", ""))
end

local validLines = {}
local lastValidLine = 0
local hooks = {}
hooks.statement = function(statement, visibleVars)
    local line = statement.FirstLine
    local data = {}
    validLines[line] = true
    lastValidLine = line
    data[#data+1] = string.format("if breakpoints[%d] or allbps then __krisDebug(%d,{", line, line)
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

local function findNextBreakLine(validLines, onLine)
    local i = onLine
    while not validLines[i] and i <= lastValidLine do
        i = i + 1
    end
    if i <= lastValidLine then
        return i
    else
        return lastValidLine
    end
end

local function findVarValue(vars, varNames, name)
    assert(#vars == #varNames, "mismatched vars and varNames tables")
    for i = 1, #varNames do
        if varNames[i] == name then
            return true, vars[i]
        end
    end
    return false
end

_ENV.breakpoints = {}
_ENV.allbps = false
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
local linesTable = split(lines, "\n")

f:close()
local ok, tree = parser(lines, {disableEmitLeadingWhite=true,
                                disableEmitTokenList=true}, hooks)
if not ok then
    error(string.format("Error parsing %s: %s", argv[1], tree))
end
local beautified = FormatBeautify(tree)
local loaded = load(beautified)

local srcAnnotated = split(lines, "\n")
for i = 1, #srcAnnotated do
    srcAnnotated[i] = string.format("%d:\t%s", i, srcAnnotated[i])
end
print(table.concat(srcAnnotated, "\n"))

local programCoro = coroutine.create(loaded)
local debugCoro = coroutine.create(function(curLine, vars, varNames)
    local function yieldToProgram()
        curLine, vars, varNames = coroutine.yield()
    end
    local cmd, lastInput
    while cmd ~= "exit" do
        io.write("> ")
        local input = io.read()
        if input == "" then
            input = lastInput or ""
        end
        local subs = split(input, "%s")
        
        cmd = subs[1]
        if cmd == "b" or cmd == "break" then
            local line = findNextBreakLine(validLines, tonumber(subs[2]))
            _ENV.breakpoints[line] = true
            print(string.format("Setting breakpoint at line %d", line))
        elseif cmd == "d" or cmd == "delete" then
            local line = tonumber(subs[2])
            if _ENV.breakpoints[line] then
                print(string.format("Deleting breakpoint at line %d", line))
                _ENV.breakpoints[line] = false
            else
                print(string.format("No breakpoint at line %d", line))
            end
        elseif cmd == "s" or cmd == "step" then
            --local count = subs[2] and tonumber(subs[2]) or 1
            _ENV.allbps = true
            --print(string.format("Setting breakpoint at line %d", line))
            yieldToProgram()
            print(strip(linesTable[curLine]))
            _ENV.allbps = false
        elseif cmd == "c" or cmd == "continue" then
            yieldToProgram()
            print(strip(linesTable[curLine]))
        elseif cmd == "p" or cmd == "print" then
            local name = subs[2]
            local found, val = findVarValue(vars, varNames, name)
            if found then
                print(val)
            else
                print(string.format("No variable named \"%s\"", name))
            end
        else
            print(string.format("Unknown command: %s", cmd))
        end
        lastInput = input
    end
end)

local ok, err
local line, vars, varnames = 1, {}, {}
while true do
    ok, err = coroutine.resume(debugCoro, line, vars, varnames)
    if not ok then
        print(err)
    end
    if coroutine.status(debugCoro) == "dead" then break end
    ok, line, vars, varnames = coroutine.resume(programCoro)
    if not ok then
        print("program stopped unexpectedly")
        break
    end
    if coroutine.status(programCoro) == "dead" then
        print("program finished execution")
        break
    end
end
    