package.loaded.ParseLua = nil
package.loaded.FormatBeautiful = nil
local parser = require("ParseLua")
local inspect = require("inspect")
local FormatBeautify = require("FormatBeautiful")

local header =
[[
local function __index(t, k)
    local v = rawget(t, k)
    if v ~= nil then
        return v
    elseif rawget(t, '.'..k) then
        return nil
    else
        t = getmetatable(t).outer
        return t[k]
    end
end

local function __newindex(t, k, v)
    if k:sub(1, 1) == '.' or rawget(t, '.'..k) or rawget(t, k) then
        rawset(t, k, v)
    else
        t = getmetatable(t).outer
        t[k] = v
    end
end
]]

header = header:gsub("\n", " ")

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

local function packSkipNils(...)
    local t = table.pack(...)
    local r = {}
    for i = 1, t.n do
        if t[i] then
            r[#r + 1] = t[i]
        end
    end
    return r
end

local filename
local returnBreakpoints = {}
local funcBeginBreakpoints = {}
local validLines = {}
local lastValidLine = 0
local hooks = {}
local bpid = 1
local lineBpMap = {}
local symtab = "__SYM"
local stacktab = "__STK"
hooks.statement = function(statement, visibleVars, innerVarsIdx, parent, isFirst, isLast)
    local line = statement.FirstLine
    local ifBreakpoint = "if breakpoints[%d] or allbps then __krisDebug(%d,%s) end"
    validLines[line] = true
    lastValidLine = line
    lineBpMap[line] = bpid

    local stackPopBeforeRet
    if statement.AstType == "ReturnStatement" then
        returnBreakpoints[#returnBreakpoints+1] = bpid
        stackPopBeforeRet = {
            AstType = "VerbatimCode",
            Data = string.format("%s[#%s] = nil", stacktab, stacktab)
        }
    end

    local setupArgs
    if parent.AstType == "Function" and isFirst then
        funcBeginBreakpoints[#funcBeginBreakpoints+1] = bpid
        local args = {}
        for _, v in ipairs(parent.Arguments) do
            args[#args + 1] = string.format("%s[\".%s\"] = true", symtab, v)
            args[#args + 1] = string.format("%s.%s = %s", symtab, v, v)
        end
        args[#args + 1] = string.format("%s.self = self", symtab)
        setupArgs = {
            AstType = "VerbatimCode",
            Data = table.concat(args, ";")
        }
    end

    local stmtPrefix = {
        AstType = "VerbatimCode",
        Data = string.format(ifBreakpoint, bpid, line, symtab),
        FirstLine = line
    }

    bpid = bpid + 1
    local stmtSuffix
    local stackPopAfterRet
    if parent.AstType == "Function" and isLast and statement.AstType ~= "ReturnStatement" then
        stmtSuffix = {
            AstType = "VerbatimCode",
            Data = string.format(ifBreakpoint, bpid, line, symtab),
            FirstLine = line
        }
        stackPopAfterRet = {
            AstType = "VerbatimCode",
            Data = string.format("%s[#%s] = nil", stacktab, stacktab)
        }
        bpid = bpid + 1
    end

    local enterFor
    if parent.AstType == "NumericForStatement" and isFirst then
        enterFor = {
            AstType = "VerbatimCode",
            Data = string.format("%s.%s = %s", symtab, parent.Variable, parent.Variable)
        }
    elseif parent.AstType == "GenericForStatement" and isFirst then
        local vars = {}
        for _, v in ipairs(parent.VariableList) do
            vars[#vars + 1] = string.format("%s.%s = %s", symtab, v, v)
        end
        enterFor = {
            AstType = "VerbatimCode",
            Data = table.concat(vars, ";")
        }
    end

    local enterMain
    if isFirst and parent.AstType == "Main" then
        enterMain = {
            AstType = "VerbatimCode",
            Data = header
        }
    end

    local enterScope
    local stackPush
    if isFirst then
        local isFuncTop = parent.AstType == "Function" or parent.AstType == "Main"
        local outer
        if parent.AstType == "Main" then
            outer = "_ENV"
        else
            outer = symtab
        end
        enterScope = {
            AstType = "VerbatimCode",
            Data = string.format(
                "local %s = setmetatable({}, {functop = %s, outer = %s, __index = __index, __newindex = __newindex})",
                symtab, tostring(isFuncTop), outer
            )
        }
        stackPush = {
            AstType = "VerbatimCode",
            Data = string.format("%s[#%s+%d] = %s", stacktab, stacktab, isFuncTop and 1 or 0, symtab)
        }
    end

    local localDecls
    if statement.AstType == "LocalStatement" then
        local localListDot = {}
        local localList = {}
        for i, v in ipairs(statement.LocalList) do
            localListDot[#localListDot + 1] = string.format("%s[\".%s\"]", symtab, v)
            localList[#localList + 1] = string.format("%s.%s", symtab, v)
        end

        localDecls = {
            AstType = "VerbatimCode",
            Data = string.format("%s = %s", table.concat(localListDot, ","), string.rep("true", #statement.LocalList, ","))
        }

        if #statement.InitList > 0 then
            statement = {
                AstType = "AssignmentStatement",
                Lhs = { {
                    AstType = "VerbatimCode",
                    Data = table.concat(localList, ",")
                } },
                Rhs = statement.InitList,
                FirstLine = line
            }
        else
            statement = {
                AstType = "VerbatimCode",
                Data = ""
            }
        end

    -- only handle functions created with syntax "local function foo(...)" - anonymous
    -- functions don't have a name (duh), we don't want to replace names of global functions
    -- and functions created with syntax "function foo:bar(...)" end up in table foo to
    -- which we will have a reference anyway
    elseif statement.AstType == "Function" and statement.IsLocal and statement.Name then
        statement.Name = {
            AstType = "MemberExpr",
            Base = {
                AstType = "VarExpr",
                Name = symtab
            },
            Ident = statement.Name,
            Indexer = "."
        }
        statement.IsLocal = false
    end

    return packSkipNils(enterMain, enterScope, stackPush, setupArgs, enterFor, stmtPrefix, localDecls, stackPopBeforeRet, statement, stmtSuffix, stackPopAfterRet)
end

hooks.varexpr = function(id, parent)
    return {AstType = "VerbatimCode", Data = string.format("%s.%s", symtab, id.Name), FirstLine = id.FirstLine}
end

local sandbox = setmetatable({}, {__index = _ENV})
local yieldToProgram

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

local function setBreakpoints(ids, state)
    for _, bpid in ipairs(ids) do
        sandbox.breakpoints[bpid] = state
    end
end

local function curStackLevel(coro)
    local level = 3
    local info = debug.getinfo(coro, level, "nSltu")
    while info do
        level = level + 1
        info = debug.getinfo(coro, level, "nSltu")
    end
    return level - 1
end

local function singleStep()
    sandbox.allbps = true
    yieldToProgram()
    sandbox.allbps = false
end

sandbox.breakpoints = {}
sandbox.allbps = false
sandbox.__krisDebug = function(line, vars)
    coroutine.yield(line, vars)
end
sandbox.__STK = {}

local argv = {...}
if argv[1] then
    filename = argv[1]
else
    error("Missing argument: filename")
end
local f = io.open(filename)
if not f then
    error(string.format("Failed opening %s", filename))
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

if argv[2] then
    local out = io.open(argv[2], "w")
    if not out then
        error(string.format("Failed opening %s", argv[2]))
    end
    out:write(beautified)
    out:close()
end

local loaded, err = load(beautified, "main chunk", "t", sandbox)
if not loaded then
    error(err)
end

local srcAnnotated = split(lines, "\n")
for i = 1, #srcAnnotated do
    srcAnnotated[i] = string.format("%d:\t%s", i, srcAnnotated[i])
end
print(table.concat(srcAnnotated, "\n"))

local programCoro = coroutine.create(loaded)
local debugCoro = coroutine.create(function(curLine, vars)
    yieldToProgram = function()
        curLine, vars = coroutine.yield()
    end
    local cmd, lastInput
    while true do
        io.write("> ")
        local input = io.read()
        if input == "" then
            input = lastInput or ""
        elseif not input then
            return
        end
        local subs = split(input, "%s")
        
        cmd = subs[1]
        if cmd == "b" or cmd == "break" then
            local line = findNextBreakLine(validLines, tonumber(subs[2]))
            sandbox.breakpoints[lineBpMap[line]] = true
            print(string.format("Setting breakpoint at line %d", line))
        elseif cmd == "d" or cmd == "delete" then
            local line = tonumber(subs[2])
            if sandbox.breakpoints[lineBpMap[line]] then
                print(string.format("Deleting breakpoint at line %d", line))
                sandbox.breakpoints[lineBpMap[line]] = false
            else
                print(string.format("No breakpoint at line %d", line))
            end
        elseif cmd == "s" or cmd == "step" then
            singleStep()
            print(strip(linesTable[curLine]))
        elseif cmd == "c" or cmd == "continue" then
            yieldToProgram()
            print(strip(linesTable[curLine]))
        elseif cmd == "p" or cmd == "print" then
            local name = subs[2]
            local val = vars[name]
            if val or vars["."..name] then
                print(val)
            else
                print(string.format("No variable named \"%s\"", name))
            end
        elseif cmd == "bt" or cmd == "backtrace" or cmd == "where" then
            local full = subs[2] == "full"
            local stklvl = #sandbox.__STK
            local level = 3
            local info = debug.getinfo(programCoro, level, "nSltu")
            while info do
                print(string.format("%s:%d: in '%s'", filename, info.currentline, info.name or "main chunk"))
                if full then
                    local locals = sandbox.__STK[stklvl]
                    while not getmetatable(locals).functop do
                        for k, v in pairs(locals) do
                            print(k, v)
                        end
                        locals = getmetatable(locals).outer
                    end
                    for k, v in pairs(locals) do
                        print(k, v)
                    end
                end
                level = level + 1
                stklvl = stklvl - 1
                info = debug.getinfo(programCoro, level, "nSltu")
            end
        elseif cmd == "l" or cmd == "list" then
            local start = subs[2] and tonumber(subs[2]) or curLine
            for i = math.max(1, start - 2), math.min(start + 2, #srcAnnotated) do
                print(srcAnnotated[i])
            end
        elseif cmd == "fin" or cmd == "finish" then
            local stklvl = curStackLevel(programCoro)
            local exitStklvl = stklvl - 1
            setBreakpoints(returnBreakpoints, true)
            singleStep() -- handle special case when we're already at the return breakpoint
            stklvl = curStackLevel(programCoro)
            while stklvl > exitStklvl do
                yieldToProgram()
                singleStep()
                stklvl = curStackLevel(programCoro)
            end
            setBreakpoints(returnBreakpoints, false)
            print(strip(linesTable[curLine]))
        elseif cmd == "rl" then
            for i,v in ipairs(returnBreakpoints) do
                print(i,v)
            end
        elseif cmd == "sl" then
            for i,v in ipairs(funcBeginBreakpoints) do
                print(i,v)
            end
        elseif cmd == "exit" then
            return
        else
            print(string.format("Unknown command: %s", cmd))
        end
        lastInput = input
    end
end)

local ok, err
local line, vars = 1, {}
while true do
    ok, err = coroutine.resume(debugCoro, line, vars)
    if not ok then
        print(err)
    end
    if coroutine.status(debugCoro) == "dead" then break end
    ok, line, vars = coroutine.resume(programCoro)
    if not ok then
        print("program stopped unexpectedly")
        break
    end
    if coroutine.status(programCoro) == "dead" then
        print("program finished execution")
        break
    end
end
    