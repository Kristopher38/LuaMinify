local util = require'Util'
local lookupify = util.lookupify

local WhiteChars = lookupify{' ', '\n', '\t', '\r'}
local EscapeLookup = {['\r'] = '\\r', ['\n'] = '\\n', ['\t'] = '\\t', ['"'] = '\\"', ["'"] = "\\'"}
local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local HexDigits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
							'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f'}

local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}

local Keywords = lookupify{
	'and', 'break', 'do', 'else', 'elseif',
	'end', 'false', 'for', 'function', 'goto', 'if',
	'in', 'local', 'nil', 'not', 'or', 'repeat',
	'return', 'then', 'true', 'until', 'while',
};

local function LexLua(src, options)
	--token dump
	options = options or {}
	local tokens = {}

	local st, err = pcall(function()
		--line / char / pointer tracking
		local p = 1
		local line = 1
		local char = 1

		--get / peek functions
		local function get()
			local c = src:sub(p,p)
			if c == '\n' then
				char = 1
				line = line + 1
			else
				char = char + 1
			end
			p = p + 1
			return c
		end
		local function peek(n)
			n = n or 0
			return src:sub(p+n,p+n)
		end
		local function consume(chars)
			local c = peek()
			for i = 1, #chars do
				if c == chars:sub(i,i) then return get() end
			end
		end

		--shared stuff
		local function generateError(err)
			return error(">> :"..line..":"..char..": "..err, 0)
		end

		local function tryGetLongString()
			local start = p
			if peek() == '[' then
				local equalsCount = 0
				local depth = 1
				while peek(equalsCount+1) == '=' do
					equalsCount = equalsCount + 1
				end
				if peek(equalsCount+1) == '[' then
					--start parsing the string. Strip the starting bit
					for _ = 0, equalsCount+1 do get() end

					--get the contents
					local contentStart = p
					while true do
						--check for eof
						if peek() == '' then
							generateError("Expected `]"..string.rep('=', equalsCount).."]` near <eof>.")
						end

						--check for the end
						local foundEnd = true
						if peek() == ']' then
							for i = 1, equalsCount do
								if peek(i) ~= '=' then foundEnd = false end
							end
							if peek(equalsCount+1) ~= ']' then
								foundEnd = false
							end
						else
							if peek() == '[' then
								-- is there an embedded long string?
								local embedded = true
								for i = 1, equalsCount do
									if peek(i) ~= '=' then
										embedded = false
										break
									end
								end
								if peek(equalsCount + 1) == '[' and embedded then
									-- oh look, there was
									depth = depth + 1
									for i = 1, (equalsCount + 2) do
										get()
									end
								end
							end
							foundEnd = false
						end
						--
						if foundEnd then
							depth = depth - 1
							if depth == 0 then
								break
							else
								for i = 1, equalsCount + 2 do
									get()
								end
							end
						else
							get()
						end
					end

					--get the interior string
					local contentString = src:sub(contentStart, p-1)

					--found the end. Get rid of the trailing bit
					for i = 0, equalsCount+1 do get() end

					--get the exterior string
					local longString = src:sub(start, p-1)

					--return the stuff
					return contentString, longString
				else
					return nil
				end
			else
				return nil
			end
		end

		--main token emitting loop
		while true do
			--get leading whitespace. The leading whitespace will include any comments
			--preceding the token. This prevents the parser needing to deal with comments
			--separately.
			local leading = { }
			local leadingWhite = ''
			local longStr = false
			while true do
				local c = peek()
				if c == '#' and peek(1) == '!' and line == 1 then
					-- #! shebang for linux scripts
					get()
					get()
					leadingWhite = "#!"
					while peek() ~= '\n' and peek() ~= '' do
						leadingWhite = leadingWhite .. get()
					end
					local token = {
						__tag = 'Comment',
						CommentType = 'Shebang',
						Data = leadingWhite,
						Line = line,
						Char = char
					}
					leadingWhite = ""
					table.insert(leading, token)
				end
				if c == ' ' or c == '\t' then
					--whitespace
					--leadingWhite = leadingWhite..get()
					local c2 = get() -- ignore whitespace
					table.insert(leading, { __tag = 'Whitespace', Line = line, Char = char, Data = c2 })
				elseif c == '\n' or c == '\r' then
					local nl = get()
					if leadingWhite ~= "" then
						local token = {
							__tag = 'Comment',
							CommentType = longStr and 'LongComment' or 'Comment',
							Data = leadingWhite,
							Line = line,
							Char = char,
						}
						table.insert(leading, token)
						leadingWhite = ""
					end
					table.insert(leading, { __tag = 'Whitespace', Line = line, Char = char, Data = nl })
				elseif c == '-' and peek(1) == '-' then
					--comment
					get()
					get()
					leadingWhite = leadingWhite .. '--'
					local _, wholeText = tryGetLongString()
					if wholeText then
						leadingWhite = leadingWhite..wholeText
						longStr = true
					else
						while peek() ~= '\n' and peek() ~= '' do
							leadingWhite = leadingWhite..get()
						end
					end
				else
					break
				end
			end
			if leadingWhite ~= "" then
				local token = {
					__tag = 'Comment',
					CommentType = longStr and 'LongComment' or 'Comment',
					Data = leadingWhite,
					Line = line,
					Char = char,
				}
				table.insert(leading, token)
			end

			--get the initial char
			local thisLine = line
			local thisChar = char
			local errorAt = ":"..line..":"..char..":> "
			local c = peek()

			--symbol to emit
			local toEmit = nil

			--branch on type
			if c == '' then
				--eof
				toEmit = { __tag = 'Eof' }

			elseif UpperChars[c] or LowerChars[c] or c == '_' then
				--ident or keyword
				local start = p
				repeat
					get()
					c = peek()
				until not (UpperChars[c] or LowerChars[c] or Digits[c] or c == '_')
				local dat = src:sub(start, p-1)
				if Keywords[dat] then
					toEmit = {__tag = 'Keyword', Data = dat}
				else
					toEmit = {__tag = 'Ident', Data = dat}
				end

			elseif Digits[c] or (peek() == '.' and Digits[peek(1)]) then
				--number const
				local start = p
				if c == '0' and peek(1) == 'x' then
					get();get()
					while HexDigits[peek()] do get() end
					if consume('Pp') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				else
					while Digits[peek()] do get() end
					if consume('.') then
						while Digits[peek()] do get() end
					end
					if consume('Ee') then
						consume('+-')
						while Digits[peek()] do get() end
					end
				end
				toEmit = {__tag = 'Number', Data = src:sub(start, p-1)}

			elseif c == '\'' or c == '\"' then
				local start = p
				--string const
				local delim = get()
				local contentStart = p
				while true do
					local c = get()
					if c == '\\' then
						get() --get the escape char
					elseif c == delim then
						break
					elseif c == '' then
						generateError("Unfinished string near <eof>")
					end
				end
				local content = src:sub(contentStart, p-2)
				local constant = src:sub(start, p-1)
				toEmit = {__tag = 'String', Data = constant, Constant = content}

			elseif c == '[' then
				local content, wholetext = tryGetLongString()
				if wholetext then
					toEmit = {__tag = 'String', Data = wholetext, Constant = content}
				else
					get()
					toEmit = {__tag = 'Symbol', Data = '['}
				end

			elseif consume('>=<') then
				if consume('=') then
					toEmit = {__tag = 'Symbol', Data = c..'='}
				else
					toEmit = {__tag = 'Symbol', Data = c}
				end

			elseif consume('~') then
				if consume('=') then
					toEmit = {__tag = 'Symbol', Data = '~='}
				else
					generateError("Unexpected symbol `~` in source.")
				end

			elseif consume('.') then
				if consume('.') then
					if consume('.') then
						toEmit = {__tag = 'Symbol', Data = '...'}
					else
						toEmit = {__tag = 'Symbol', Data = '..'}
					end
				else
					toEmit = {__tag = 'Symbol', Data = '.'}
				end

			elseif consume(':') then
				if consume(':') then
					toEmit = {__tag = 'Symbol', Data = '::'}
				else
					toEmit = {__tag = 'Symbol', Data = ':'}
				end

			elseif Symbols[c] then
				get()
				toEmit = {__tag = 'Symbol', Data = c}

			else
				local contents, all = tryGetLongString()
				if contents then
					toEmit = {__tag = 'String', Data = all, Constant = contents}
				else
					generateError("Unexpected Symbol `"..c.."` in source.")
				end
			end

			--add the emitted symbol, after adding some common data
			if not options.disableEmitLeadingWhite then
				toEmit.LeadingWhite = leading -- table of leading whitespace/comments
			end
			--for k, tok in pairs(leading) do
			--	tokens[#tokens + 1] = tok
			--end

			toEmit.Line = thisLine
			toEmit.Char = thisChar
			tokens[#tokens+1] = toEmit

			--halt after eof has been emitted
			if toEmit and toEmit.__tag == 'Eof' then break end
		end
	end)
	if not st then
		return false, err
	end

	--public interface:
	local tok = {}
	local savedP = {}
	local p = 1
	
	function tok:getp()
		return p
	end
	
	function tok:setp(n)
		p = n
	end
	
	function tok:getTokenList()
		return tokens
	end
	
	--getters
	function tok:Peek(n)
		n = n or 0
		return tokens[math.min(#tokens, p+n)]
	end
	function tok:Get(tokenList)
		local t = tokens[p]
		p = math.min(p + 1, #tokens)
		if tokenList then
			table.insert(tokenList, t)
		end
		return t
	end
	function tok:Is(t)
		return tok:Peek().__tag == t
	end

	--save / restore points in the stream
	function tok:Save()
		savedP[#savedP+1] = p
	end
	function tok:Commit()
		savedP[#savedP] = nil
	end
	function tok:Restore()
		p = savedP[#savedP]
		savedP[#savedP] = nil
	end

	--either return a symbol if there is one, or return true if the requested
	--symbol was gotten.
	function tok:ConsumeSymbol(symb, tokenList)
		local t = self:Peek()
		if t.__tag == 'Symbol' then
			if symb then
				if t.Data == symb then
					self:Get(tokenList)
					return true
				else
					return nil
				end
			else
				self:Get(tokenList)
				return t
			end
		else
			return nil
		end
	end

	function tok:ConsumeKeyword(kw, tokenList)
		local t = self:Peek()
		if t.__tag == 'Keyword' and t.Data == kw then
			self:Get(tokenList)
			return true
		else
			return nil
		end
	end

	function tok:IsKeyword(kw)
		local t = tok:Peek()
		return t.__tag == 'Keyword' and t.Data == kw
	end

	function tok:IsSymbol(s)
		local t = tok:Peek()
		return t.__tag == 'Symbol' and t.Data == s
	end

	function tok:IsEof()
		return tok:Peek().__tag == 'Eof'
	end

	return true, tok
end


local amuletRecordMt = {
	__index = function(t, k)
		if k == 1 then
			return t
		else
            error(string.format("attempting to index with nonexistent key %s in table %s", k, require("inspect")(t)))
			return nil
		end
	end,
	__newindex = function(t, k, v)
		if k == 1 then
			error(string.format("assignment to [1] in table %s", require("inspect")(t)))
		else
			rawset(t, k, v)
		end
	end
}


local function ParseLua(src, options, hooks)
	options = options or {}
	hooks = hooks or {}
	hooks.statement = hooks.statement or function(statement, ...) return {statement} end
	hooks.func = hooks.func or function(...) end
	hooks.varexpr = hooks.varexpr or function(expr, ...) return expr end

	local st, tok
	if type(src) ~= 'table' then
		st, tok = LexLua(src, options)
	else
		st, tok = true, src
	end
	if not st then
		return false, tok
	end
	--
	local function GenerateError(msg)
		local err = ">> :"..tok:Peek().Line..":"..tok:Peek().Char..": "..msg.."\n"
		--find the line
		local lineNum = 0
		if type(src) == 'string' then
			for line in src:gmatch("[^\n]*\n?") do
				if line:sub(-1,-1) == '\n' then line = line:sub(1,-2) end
				lineNum = lineNum+1
				if lineNum == tok:Peek().Line then
					err = err..">> `"..line:gsub('\t','    ').."`\n"
					for i = 1, tok:Peek().Char do
						local c = line:sub(i,i)
						if c == '\t' then
							err = err..'    '
						else
							err = err..' '
						end
					end
					err = err.."   ^^^^"
					break
				end
			end
		end
		return err
	end

	local function getFirstLine(tokenList)
		if tokenList[1] then
			return tokenList[1].Line
		else
			error("No first line")
		end
	end
	
	local curScopeVars = {}
	local function addVar(name)
		curScopeVars[#curScopeVars+1] = name
	end
	local function flushScope(savedp)
		for i = #curScopeVars, savedp+1, -1 do
			curScopeVars[i] = nil
		end
	end

	local ParseExpr
	local ParseStatementList
	local ParseSimpleExpr, 
			ParseSubExpr,
			ParsePrimaryExpr,
			ParseSuffixedExpr

	local function ParseFunctionArgsAndBody(tokenList)
		if not tok:ConsumeSymbol('(', tokenList) then
			return false, GenerateError("`(` expected.")
		end

		--arg list
		local argList = {}
		local isVarArg = false
		local savedp = #curScopeVars
		while not tok:ConsumeSymbol(')', tokenList) do
			if tok:Is('Ident') then
				local arg = tok:Get(tokenList).Data
				argList[#argList+1] = arg
				addVar(arg)
				if not tok:ConsumeSymbol(',', tokenList) then
					if tok:ConsumeSymbol(')', tokenList) then
						break
					else
						return false, GenerateError("`)` expected.")
					end
				end
			elseif tok:ConsumeSymbol('...', tokenList) then
				isVarArg = true
				addVar("...")
				if not tok:ConsumeSymbol(')', tokenList) then
					return false, GenerateError("`...` must be the last argument of a function.")
				end
				break
			else
				return false, GenerateError("Argument name or `...` expected")
			end
		end

		local nodeFunc = setmetatable({}, amuletRecordMt)
		nodeFunc.args = argList
		nodeFunc.vararg    = isVarArg

		--body
		local st, body = ParseStatementList(nodeFunc)
		if not st then return false, body end

		--end
		if not tok:ConsumeKeyword('end', tokenList) then
			return false, GenerateError("`end` expected after function body")
		end

		nodeFunc.body = body
		if not options.disableEmitTokenList then
			nodeFunc.tokens    = tokenList
		end

		hooks.func(nodeFunc, curScopeVars)
		flushScope(savedp)
		return true, nodeFunc
	end


	function ParsePrimaryExpr(parent)
		local tokenList = {}

		if tok:ConsumeSymbol('(', tokenList) then
			local st, ex = ParseExpr(parent)
			if not st then return false, ex end
			if not tok:ConsumeSymbol(')', tokenList) then
				return false, GenerateError("`)` Expected.")
			end
			local parensExp = setmetatable({}, amuletRecordMt)
			parensExp.__tag   = 'ParenExpr'
			parensExp.inner     = ex
			if not options.disableEmitTokenList then
				parensExp.tokens    = tokenList
			end
			parensExp.first_line = getFirstLine(tokenList)
			return true, parensExp
		elseif tok:Is('Ident') then
			local id = tok:Get(tokenList)
			--
			local nodePrimExp = setmetatable({}, amuletRecordMt)
			nodePrimExp.__tag   = 'VarExpr'
			nodePrimExp.name      = id.Data
			if not options.disableEmitTokenList then
				nodePrimExp.tokens    = tokenList
			end
			nodePrimExp.first_line = getFirstLine(tokenList)
			--
			return true, hooks.varexpr(nodePrimExp, parent)
		else
			return false, GenerateError("primary expression expected")
		end
	end

	function ParseSuffixedExpr(onlyDotColon, parent)
		--base primary expression
		local st, prim = ParsePrimaryExpr(parent)
		if not st then return false, prim end
		--
		while true do
			local tokenList = {}

			if tok:IsSymbol('.') or tok:IsSymbol(':') then
				local symb = tok:Get(tokenList).Data
				if not tok:Is('Ident') then
					return false, GenerateError("<Ident> expected.")
				end
				local id = tok:Get(tokenList).Data
				local nodeIndex = setmetatable({}, amuletRecordMt)
				nodeIndex.__tag  = 'MemberExpr'
				nodeIndex.base     = prim
				nodeIndex.indexer  = symb
				nodeIndex.ident    = id
				if not options.disableEmitTokenList then
					nodeIndex.tokens   = tokenList
				end
				nodeIndex.first_line = getFirstLine(tokenList)
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('[', tokenList) then
				local st, ex = ParseExpr(parent)
				if not st then return false, ex end
				if not tok:ConsumeSymbol(']', tokenList) then
					return false, GenerateError("`]` expected.")
				end
				local nodeIndex = setmetatable({}, amuletRecordMt)
				nodeIndex.__tag  = 'IndexExpr'
				nodeIndex.base     = prim
				nodeIndex.index    = ex
				if not options.disableEmitTokenList then
					nodeIndex.tokens   = tokenList
				end
				nodeIndex.first_line = getFirstLine(tokenList)
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('(', tokenList) then
				local args = {}
				while not tok:ConsumeSymbol(')', tokenList) do
					local st, ex = ParseExpr(parent)
					if not st then return false, ex end
					args[#args+1] = ex
					if not tok:ConsumeSymbol(',', tokenList) then
						if tok:ConsumeSymbol(')', tokenList) then
							break
						else
							return false, GenerateError("`)` Expected.")
						end
					end
				end
				local nodeCall = setmetatable({}, amuletRecordMt)
				nodeCall.__tag   = 'CallExpr'
				nodeCall.base      = prim
				nodeCall.args = args
				if not options.disableEmitTokenList then
					nodeCall.tokens    = tokenList
				end
				nodeCall.first_line = getFirstLine(tokenList)
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:Is('String') then
				--string call
				local nodeCall = setmetatable({}, amuletRecordMt)
				nodeCall.__tag    = 'StringCallExpr'
				nodeCall.base       = prim
				nodeCall.arg  = tok:Get(tokenList).Data
				if not options.disableEmitTokenList then
					nodeCall.tokens     = tokenList
				end
				nodeCall.first_line = getFirstLine(tokenList)
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:IsSymbol('{') then
				--table call
				local st, ex = ParseSimpleExpr(parent)
				-- TODO: ParseExpr(parent) parses the table AND and any following binary expressions.
				-- We just want the table
				if not st then return false, ex end
				local nodeCall = setmetatable({}, amuletRecordMt)
				nodeCall.__tag   = 'TableCallExpr'
				nodeCall.base      = prim
				nodeCall.arg = ex
				if not options.disableEmitTokenList then
					nodeCall.tokens    = tokenList
				end
				nodeCall.first_line = prim.first_line
				--
				prim = nodeCall

			else
				break
			end
		end
		return true, prim
	end


	function ParseSimpleExpr(parent)
		local tokenList = {}

		if tok:Is('Number') then
			local nodeNum = setmetatable({}, amuletRecordMt)
			nodeNum.__tag = 'NumberExpr'
			nodeNum.value   = tonumber(tok:Get(tokenList).Data)
			if not options.disableEmitTokenList then
				nodeNum.tokens  = tokenList
			end
			nodeNum.first_line = getFirstLine(tokenList)
			return true, nodeNum

		elseif tok:Is('String') then
			local nodeStr = setmetatable({}, amuletRecordMt)
			nodeStr.__tag = 'StringExpr'
			nodeStr.value   = tok:Get(tokenList).Data
			if not options.disableEmitTokenList then
				nodeStr.tokens  = tokenList
			end
			nodeStr.first_line = getFirstLine(tokenList)
			return true, nodeStr

		elseif tok:ConsumeKeyword('nil', tokenList) then
			local nodeNil = setmetatable({}, amuletRecordMt)
			nodeNil.__tag = 'NilExpr'
			if not options.disableEmitTokenList then
				nodeNil.tokens  = tokenList
			end
			nodeNil.first_line = getFirstLine(tokenList)
			return true, nodeNil

		elseif tok:IsKeyword('false') or tok:IsKeyword('true') then
			local nodeBoolean = setmetatable({}, amuletRecordMt)
			nodeBoolean.__tag = 'BooleanExpr'
			nodeBoolean.value   = (tok:Get(tokenList).Data == 'true')
			if not options.disableEmitTokenList then
				nodeBoolean.tokens  = tokenList
			end
			nodeBoolean.first_line = getFirstLine(tokenList)
			return true, nodeBoolean

		elseif tok:ConsumeSymbol('...', tokenList) then
			local nodeDots = setmetatable({}, amuletRecordMt)
			nodeDots.__tag  = 'DotsExpr'
			if not options.disableEmitTokenList then
				nodeDots.tokens   = tokenList
			end
			nodeDots.first_line = getFirstLine(tokenList)
			return true, nodeDots

		elseif tok:ConsumeSymbol('{', tokenList) then
			local v = setmetatable({}, amuletRecordMt)
			v.__tag = 'ConstructorExpr'
			v.entry_list = {}
			--
			while true do
				if tok:IsSymbol('[') then
					--key
					tok:Get(tokenList)
					local st, key = ParseExpr(parent)
					if not st then
						return false, GenerateError("Key expr Expected")
					end
					if not tok:ConsumeSymbol(']', tokenList) then
						return false, GenerateError("`]` Expected")
					end
					if not tok:ConsumeSymbol('=', tokenList) then
						return false, GenerateError("`=` Expected")
					end
					local st, value = ParseExpr(parent)
					if not st then
						return false, GenerateError("value expr Expected")
					end
					v.entry_list[#v.entry_list+1] = setmetatable({
						__tag  = 'Key';
						key   = key;
						value = value;
					}, amuletRecordMt)

				elseif tok:Is('Ident') then
					--value or key
					local lookahead = tok:Peek(1)
					if lookahead.__tag == 'Symbol' and lookahead.Data == '=' then
						--we are a key
						local key = tok:Get(tokenList)
						if not tok:ConsumeSymbol('=', tokenList) then
							return false, GenerateError("`=` Expected")
						end
						local st, value = ParseExpr(parent)
						if not st then
							return false, GenerateError("value expr Expected")
						end
						v.entry_list[#v.entry_list+1] = setmetatable({
							__tag  = 'KeyString';
							key   = key.Data;
							value = value;
						}, amuletRecordMt)

					else
						--we are a value
						local st, value = ParseExpr(parent)
						if not st then
							return false, GenerateError("value Exected")
						end
						v.entry_list[#v.entry_list+1] = setmetatable({
							__tag = 'Value';
							value = value;
						}, amuletRecordMt)

					end
				elseif tok:ConsumeSymbol('}', tokenList) then
					break

				else
					--value
					local st, value = ParseExpr(parent)
					v.entry_list[#v.entry_list+1] = setmetatable({
						__tag = 'Value';
						value = value;
					}, amuletRecordMt)
					if not st then
						return false, GenerateError("value Expected")
					end
				end

				if tok:ConsumeSymbol(';', tokenList) or tok:ConsumeSymbol(',', tokenList) then
					--all is good
				elseif tok:ConsumeSymbol('}', tokenList) then
					break
				else
					return false, GenerateError("`}` or table entry Expected")
				end
			end
			if not options.disableEmitTokenList then
				v.tokens  = tokenList
			end
			v.first_line = getFirstLine(tokenList)
			return true, v

		elseif tok:ConsumeKeyword('function', tokenList) then
			local st, func = ParseFunctionArgsAndBody(tokenList)
			if not st then return false, func end
			--
			func.__tag   = 'FunctionExpr'
			func.first_line = getFirstLine(tokenList)
			return true, func

		else
			return ParseSuffixedExpr(false, parent)
		end
	end


	local unops = lookupify{'-', 'not', '#'}
	local unopprio = 8
	local priority = {
		['+'] = {6,6};
		['-'] = {6,6};
		['%'] = {7,7};
		['/'] = {7,7};
		['*'] = {7,7};
		['^'] = {10,9};
		['..'] = {5,4};
		['=='] = {3,3};
		['<'] = {3,3};
		['<='] = {3,3};
		['~='] = {3,3};
		['>'] = {3,3};
		['>='] = {3,3};
		['and'] = {2,2};
		['or'] = {1,1};
	}
	function ParseSubExpr(level, parent)
		--base item, possibly with unop prefix
		local st, exp
		if unops[tok:Peek().Data] then
			local tokenList = {}
			local op = tok:Get(tokenList).Data
			st, exp = ParseSubExpr(unopprio, parent)
			if not st then return false, exp end
			local nodeEx = setmetatable({}, amuletRecordMt)
			nodeEx.__tag = 'UnopExpr'
			nodeEx.rhs     = exp
			nodeEx.op      = op
			nodeEx.op_prec = unopprio
			if not options.disableEmitTokenList then
				nodeEx.tokens  = tokenList
			end
			nodeEx.first_line = getFirstLine(tokenList)
			exp = nodeEx
		else
			st, exp = ParseSimpleExpr(parent)
			if not st then return false, exp end
		end

		--next items in chain
		while true do
			local prio = priority[tok:Peek().Data]
			if prio and prio[1] > level then
				local tokenList = {}
				local op = tok:Get(tokenList).Data
				local st, rhs = ParseSubExpr(prio[2], parent)
				if not st then return false, rhs end
				local nodeEx = setmetatable({}, amuletRecordMt)
				nodeEx.__tag = 'BinopExpr'
				nodeEx.lhs     = exp
				nodeEx.op      = op
				nodeEx.op_prec = prio[1]
				nodeEx.rhs     = rhs
				if not options.disableEmitTokenList then
					nodeEx.tokens  = tokenList
				end
				nodeEx.first_line = getFirstLine(tokenList)
				--
				exp = nodeEx
			else
				break
			end
		end

		return true, exp
	end


	ParseExpr = function(parent)
		return ParseSubExpr(0, parent)
	end


	local function ParseStatement()
		local stat = nil
		local tokenList = {}
		if tok:ConsumeKeyword('if', tokenList) then
			--setup
			local nodeIfStat = setmetatable({}, amuletRecordMt)
			nodeIfStat.__tag = 'IfStatement'
			nodeIfStat.clauses = {}
			nodeIfStat.first_line = getFirstLine(tokenList)

			--clauses
			repeat
				local st, nodeCond = ParseExpr(nodeIfStat)
				if not st then return false, nodeCond end
				if not tok:ConsumeKeyword('then', tokenList) then
					return false, GenerateError("`then` expected.")
				end
				local st, nodeBody = ParseStatementList(nodeIfStat)
				if not st then return false, nodeBody end
				nodeIfStat.clauses[#nodeIfStat.clauses+1] = {
					cond = nodeCond;
					body = nodeBody;
				}
			until not tok:ConsumeKeyword('elseif', tokenList)

			--else clause
			if tok:ConsumeKeyword('else', tokenList) then
				local st, nodeBody = ParseStatementList(nodeIfStat)
				if not st then return false, nodeBody end
				nodeIfStat.else_body = {__tag = "Some", nodeBody}
			else
				nodeIfStat.else_body = {__tag = "None"}
			end

			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end
			if not options.disableEmitTokenList then
				nodeIfStat.tokens = tokenList
			end
			stat = nodeIfStat

		elseif tok:ConsumeKeyword('while', tokenList) then
			--setup
			local nodeWhileStat = setmetatable({}, amuletRecordMt)
			nodeWhileStat.__tag = 'WhileStatement'
			nodeWhileStat.first_line = getFirstLine(tokenList)

			--condition
			local st, nodeCond = ParseExpr(nodeWhileStat)
			if not st then return false, nodeCond end

			--do
			if not tok:ConsumeKeyword('do', tokenList) then
				return false, GenerateError("`do` expected.")
			end

			--body
			local st, nodeBody = ParseStatementList(nodeWhileStat)
			if not st then return false, nodeBody end

			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			--return
			nodeWhileStat.cond = nodeCond
			nodeWhileStat.body      = nodeBody
			if not options.disableEmitTokenList then
				nodeWhileStat.tokens    = tokenList
			end
			stat = nodeWhileStat

		elseif tok:ConsumeKeyword('do', tokenList) then
			--do block
			local nodeDoStat = setmetatable({}, amuletRecordMt)
			nodeDoStat.__tag = 'DoStatement'

			local st, nodeBlock = ParseStatementList(nodeDoStat)
			if not st then return false, nodeBlock end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			nodeDoStat.body    = nodeBlock
			if not options.disableEmitTokenList then
				nodeDoStat.tokens  = tokenList
			end
			nodeDoStat.first_line = getFirstLine(tokenList)
			stat = nodeDoStat

		elseif tok:ConsumeKeyword('for', tokenList) then
			--for block
			if not tok:Is('Ident') then
				return false, GenerateError("<Ident> expected.")
			end
			local baseVarName = tok:Get(tokenList)
			if tok:ConsumeSymbol('=', tokenList) then
				--numeric for
				local savedp = #curScopeVars
				local forVar = baseVarName.Data
				addVar(forVar)
				--
				local nodeFor = setmetatable({}, amuletRecordMt)
				nodeFor.__tag  = 'NumericForStatement'
				nodeFor.var = forVar

				local st, startEx = ParseExpr(nodeFor)
				nodeFor.start    = startEx
				if not st then return false, startEx end
				if not tok:ConsumeSymbol(',', tokenList) then
					return false, GenerateError("`,` Expected")
				end
				local st, endEx = ParseExpr(nodeFor)
				nodeFor.finish      = endEx
				if not st then return false, endEx end
				local st, stepEx;
				if tok:ConsumeSymbol(',', tokenList) then
					st, stepEx = ParseExpr(nodeFor)
					if not st then return false, stepEx end
				end
				if stepEx then
					nodeFor.step = {__tag = "Some", stepEx}
				else
					nodeFor.step = {__tag = "None"}
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected")
				end

				local st, body = ParseStatementList(nodeFor)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected")
				end
				flushScope(savedp)
				--
				nodeFor.body     = body
				if not options.disableEmitTokenList then
					nodeFor.tokens   = tokenList
				end
				nodeFor.first_line = getFirstLine(tokenList)
				stat = nodeFor
			else
				--generic for
				--
				local savedp = #curScopeVars
				local varList = { baseVarName.Data }
				addVar(baseVarName.Data)

				local nodeFor = setmetatable({}, amuletRecordMt)
				nodeFor.__tag      = 'GenericForStatement'
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("for variable expected.")
					end
					varList[#varList+1] = tok:Get(tokenList).Data
					addVar(varList[#varList])
				end
				if not tok:ConsumeKeyword('in', tokenList) then
					return false, GenerateError("`in` expected.")
				end
				nodeFor.var_list = varList
				local generators = {}
				local st, firstGenerator = ParseExpr(nodeFor)
				if not st then return false, firstGenerator end
				generators[#generators+1] = firstGenerator
				while tok:ConsumeSymbol(',', tokenList) do
					local st, gen = ParseExpr(nodeFor)
					if not st then return false, gen end
					generators[#generators+1] = gen
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected.")
				end

				nodeFor.generators   = generators

				local st, body = ParseStatementList(nodeFor)
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected.")
				end
				flushScope(savedp)
				--
				nodeFor.body         = body
				if not options.disableEmitTokenList then
					nodeFor.tokens       = tokenList
				end
				nodeFor.first_line = getFirstLine(tokenList)
				stat = nodeFor
			end

		elseif tok:ConsumeKeyword('repeat', tokenList) then
			local nodeRepeat = setmetatable({}, amuletRecordMt)
			nodeRepeat.__tag   = 'RepeatStatement'
			local st, body = ParseStatementList(nodeRepeat)
			if not st then return false, body end
			nodeRepeat.body      = body
			--
			if not tok:ConsumeKeyword('until', tokenList) then
				return false, GenerateError("`until` expected.")
			end
			-- FIX: Used to parse in parent scope
			-- Now parses in repeat scope
			local st, cond = ParseExpr(nodeRepeat)
			if not st then return false, cond end
			--
			nodeRepeat.cond = cond
			if not options.disableEmitTokenList then
				nodeRepeat.tokens    = tokenList
			end
			nodeRepeat.first_line = getFirstLine(tokenList)
			stat = nodeRepeat

		elseif tok:ConsumeKeyword('function', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Function name expected")
			end
			local st, name = ParseSuffixedExpr(true, setmetatable({__tag = "FunctionExpr"}, amuletRecordMt)) --true => only dots and colons
			if not st then return false, name end
			--
			local st, func = ParseFunctionArgsAndBody(tokenList)
			if not st then return false, func end
			--
			stat = {
				__tag = 'FunctionStatement',
				name = name,
				is_local = false,
				f = func,
				first_line = getFirstLine(tokenList)
			}

		elseif tok:ConsumeKeyword('local', tokenList) then
			if tok:Is('Ident') then
				local nodeLocal = setmetatable({}, amuletRecordMt)
				nodeLocal.__tag   = 'LocalStatement'
				local varList = { tok:Get(tokenList).Data }
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("local var name expected")
					end
					varList[#varList+1] = tok:Get(tokenList).Data
				end
				nodeLocal.names = varList

				local initList = {}
				if tok:ConsumeSymbol('=', tokenList) then
					repeat
						local st, ex = ParseExpr(nodeLocal)
						if not st then return false, ex end
						initList[#initList+1] = ex
					until not tok:ConsumeSymbol(',', tokenList)
				end

				nodeLocal.init_exprs  = initList
				if not options.disableEmitTokenList then
					nodeLocal.tokens    = tokenList
				end
				nodeLocal.first_line = getFirstLine(tokenList)
				--
				stat = nodeLocal

			elseif tok:ConsumeKeyword('function', tokenList) then
				if not tok:Is('Ident') then
					return false, GenerateError("Function name expected")
				end
				local savedp = #curScopeVars
				local name = tok:Get(tokenList).Data
				addVar(name)
				--
				local st, func = ParseFunctionArgsAndBody(tokenList)
				if not st then return false, func end
				flushScope(savedp)
				-- 
				stat = {
					__tag = 'FunctionStatement',
					name = name,
					is_local = true,
					f = func,
					first_line = getFirstLine(tokenList)
				}

			else
				return false, GenerateError("local var or function def expected")
			end

		elseif tok:ConsumeSymbol('::', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError('label name expected')
			end
			local label = tok:Get(tokenList).Data
			if not tok:ConsumeSymbol('::', tokenList) then
				return false, GenerateError("`::` expected")
			end
			local nodeLabel = setmetatable({}, amuletRecordMt)
			nodeLabel.__tag = 'LabelStatement'
			nodeLabel.label   = label
			if not options.disableEmitTokenList then
				nodeLabel.tokens  = tokenList
			end
			nodeLabel.first_line = getFirstLine(tokenList)
			stat = nodeLabel

		elseif tok:ConsumeKeyword('return', tokenList) then
			local nodeReturn = setmetatable({}, amuletRecordMt)
			nodeReturn.__tag   = 'ReturnStatement'
			local exList = {}
			if not tok:IsKeyword('end') then
				local st, firstEx = ParseExpr(nodeReturn)
				if st then
					exList[1] = firstEx
					while tok:ConsumeSymbol(',', tokenList) do
						local st, ex = ParseExpr(nodeReturn)
						if not st then return false, ex end
						exList[#exList+1] = ex
					end
				end
			end

			nodeReturn.args = exList
			if not options.disableEmitTokenList then
				nodeReturn.tokens    = tokenList
			end
			nodeReturn.first_line = getFirstLine(tokenList)
			stat = nodeReturn

		elseif tok:ConsumeKeyword('break', tokenList) then
			local nodeBreak = setmetatable({}, amuletRecordMt)
			nodeBreak.__tag = 'BreakStatement'
			if not options.disableEmitTokenList then
				nodeBreak.tokens  = tokenList
			end
			nodeBreak.first_line = getFirstLine(tokenList)
			stat = nodeBreak

		elseif tok:ConsumeKeyword('goto', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("label expected")
			end
			local label = tok:Get(tokenList).Data
			local nodeGoto = setmetatable({}, amuletRecordMt)
			nodeGoto.__tag = 'GotoStatement'
			nodeGoto.label   = label
			if not options.disableEmitTokenList then
				nodeGoto.tokens  = tokenList
			end
			nodeGoto.first_line = getFirstLine(tokenList)
			stat = nodeGoto

		else
			--statementParseExpr
			local st, suffixed = ParseSuffixedExpr(false, {})
			if not st then return false, suffixed end

			--assignment or call?
			if tok:IsSymbol(',') or tok:IsSymbol('=') then
				local nodeAssign = setmetatable({}, amuletRecordMt)
				nodeAssign.__tag = 'AssignmentStatement'
				--check that it was not parenthesized, making it not an lvalue
				if suffixed.__tag == 'ParenExpr' then
					return false, GenerateError("Can not assign to parenthesized expression, is not an lvalue")
				end

				--more processing needed
				local lhs = { suffixed }
				while tok:ConsumeSymbol(',', tokenList) do
					local st, lhsPart = ParseSuffixedExpr(false, nodeAssign)
					if not st then return false, lhsPart end
					lhs[#lhs+1] = lhsPart
				end
				nodeAssign.lhs     = lhs

				--equals
				if not tok:ConsumeSymbol('=', tokenList) then
					return false, GenerateError("`=` Expected.")
				end

				--rhs
				local rhs = {}
				local st, firstRhs = ParseExpr(nodeAssign)
				if not st then return false, firstRhs end
				rhs[1] = firstRhs
				while tok:ConsumeSymbol(',', tokenList) do
					local st, rhsPart = ParseExpr(nodeAssign)
					if not st then return false, rhsPart end
					rhs[#rhs+1] = rhsPart
				end

				--done
				nodeAssign.rhs     = rhs
				if not options.disableEmitTokenList then
					nodeAssign.tokens  = tokenList
				end
				nodeAssign.first_line = getFirstLine(tokenList)
				stat = nodeAssign

			elseif suffixed.__tag == 'CallExpr' or
				   suffixed.__tag == 'TableCallExpr' or
				   suffixed.__tag == 'StringCallExpr'
			then
				--it's a call statement
				local nodeCall = suffixed
				nodeCall.__tag    = 'CallStatement'
				if not options.disableEmitTokenList then
					nodeCall.tokens     = tokenList
				end
				nodeCall.first_line = suffixed.base.first_line
				stat = nodeCall
			else
				return false, GenerateError("Assignment Statement Expected")
			end
		end

		if tok:IsSymbol(';') then
			if options.disableEmitTokenList then
				tok:Get(nil)
			else
				tok:Get(stat.tokens)
			end
		end
		return true, setmetatable(stat, amuletRecordMt)
	end


	local statListCloseKeywords = lookupify{'end', 'else', 'elseif', 'until'}

	ParseStatementList = function(parent)
		local nodeStatlist = {}

		local st, nodeStatement
		local savedp = #curScopeVars
		local isFirst = true
		while not statListCloseKeywords[tok:Peek().Data] and not tok:IsEof() do
			st, nodeStatement = ParseStatement()
			if not st then return false, nodeStatement end
 
			local isLast = statListCloseKeywords[tok:Peek().Data] or tok:IsEof()
			for _, stmt in ipairs(hooks.statement(nodeStatement, curScopeVars, savedp, parent, isFirst, isLast)) do
				nodeStatlist[#nodeStatlist + 1] = stmt
			end
			isFirst = false

			-- variables decleared as local aren't in scope when defining them
			-- so they should be added to scope only after they're fully defined
			if nodeStatement.__tag == "LocalStatement" then
				for i = 1, #nodeStatement.names do
					addVar(nodeStatement.names[i])
				end
			end
		end

		if tok:IsEof() then
			local nodeEof = setmetatable({}, amuletRecordMt)
			nodeEof.__tag = 'Eof'
			if not options.disableEmitTokenList then
				nodeEof.tokens  = { tok:Get() }
			end
			nodeStatlist[#nodeStatlist + 1] = nodeEof
		end

		-- remove variables when leaving scope
		flushScope(savedp)

		return true, nodeStatlist
	end


	local function mainfunc()
		return ParseStatementList(setmetatable({
			__tag = "Main"
		}, amuletRecordMt))
	end

	local st, main = mainfunc()
	--print("Last Token: "..PrintTable(tok:Peek()))
	return st, main
end

return ParseLua
	
