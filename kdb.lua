package.loaded.ParseLua = nil
package.loaded.FormatBeautiful = nil
local parser = require("ParseLua")
local inspect = require("inspect")
local FormatBeautify = require("FormatBeautiful")

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

local argv = {...}
local hooks = {}

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
    out:write(inspect(tree))
    out:close()
end

local loaded, err = load(beautified, "main chunk", "t", sandbox)
if not loaded then
    error(err)
end

