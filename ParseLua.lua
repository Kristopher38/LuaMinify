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

local Parser = {}
local haxe = {}

function Parser.Init(array, option, stmt, expr, tableElem, functionDef)
	haxe.array = array
	haxe.option = option
	haxe.stmt = stmt
	haxe.expr = expr
	haxe.tableElem = tableElem
	haxe.functionDef = functionDef
end

function Parser.LexLua(src, options)
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


function Parser.ParseLua(src, options, hooks)
	options = options or {}
	hooks = hooks or {}
	hooks.statement = hooks.statement or function(statement, ...) return {statement} end
	hooks.func = hooks.func or function(...) end
	hooks.varexpr = hooks.varexpr or function(expr, ...) return expr end

	local st, tok
	if type(src) ~= 'table' then
		st, tok = Parser.LexLua(src, options)
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
		local argList = haxe.array.new()
		local isVarArg = false
		local savedp = #curScopeVars
		while not tok:ConsumeSymbol(')', tokenList) do
			if tok:Is('Ident') then
				local arg = tok:Get(tokenList).Data
				argList:push(arg)
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

		--body
		local st, body = ParseStatementList()
		if not st then return false, body end

		--end
		if not tok:ConsumeKeyword('end', tokenList) then
			return false, GenerateError("`end` expected after function body")
		end

		local nodeFunc = haxe.functionDef.FuncDef(argList, isVarArg, body)
		if not options.disableEmitTokenList then
			nodeFunc.tokens    = tokenList
		end

		hooks.func(nodeFunc, curScopeVars)
		flushScope(savedp)
		return true, nodeFunc
	end


	function ParsePrimaryExpr()
		local tokenList = {}

		if tok:ConsumeSymbol('(', tokenList) then
			local st, inner = ParseExpr()
			if not st then return false, inner end
			if not tok:ConsumeSymbol(')', tokenList) then
				return false, GenerateError("`)` Expected.")
			end
			rawset(inner, "parenthesized", true)
			return true, inner
		elseif tok:Is('Ident') then
			local id = tok:Get(tokenList)
			--
			local nodePrimExp = haxe.expr.VarExpr(id.Data)
			if not options.disableEmitTokenList then
				nodePrimExp.tokens    = tokenList
			end
			rawset(nodePrimExp, "first_line", getFirstLine(tokenList))
			--
			return true, hooks.varexpr(nodePrimExp)
		else
			return false, GenerateError("primary expression expected")
		end
	end

	function ParseSuffixedExpr(onlyDotColon)
		--base primary expression
		local st, prim = ParsePrimaryExpr()
		if not st then return false, prim end
		--
		while true do
			local tokenList = {}

			if tok:IsSymbol('.') or tok:IsSymbol(':') then
				local indexer = tok:Get(tokenList).Data
				if not tok:Is('Ident') then
					return false, GenerateError("<Ident> expected.")
				end
				local ident = tok:Get(tokenList).Data
				local nodeIndex = haxe.expr.MemberExpr(prim, indexer, ident)
				if not options.disableEmitTokenList then
					nodeIndex.tokens   = tokenList
				end
				rawset(nodeIndex, "first_line", getFirstLine(tokenList))
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('[', tokenList) then
				local st, index = ParseExpr()
				if not st then return false, index end
				if not tok:ConsumeSymbol(']', tokenList) then
					return false, GenerateError("`]` expected.")
				end
				local nodeIndex = haxe.expr.IndexExpr(prim, index)
				if not options.disableEmitTokenList then
					nodeIndex.tokens   = tokenList
				end
				rawset(nodeIndex, "first_line", getFirstLine(tokenList))
				--
				prim = nodeIndex

			elseif not onlyDotColon and tok:ConsumeSymbol('(', tokenList) then
				local args = haxe.array.new()
				while not tok:ConsumeSymbol(')', tokenList) do
					local st, ex = ParseExpr()
					if not st then return false, ex end
					args:push(ex)
					if not tok:ConsumeSymbol(',', tokenList) then
						if tok:ConsumeSymbol(')', tokenList) then
							break
						else
							return false, GenerateError("`)` Expected.")
						end
					end
				end
				local nodeCall = haxe.expr.CallExpr(prim, args)
				if not options.disableEmitTokenList then
					nodeCall.tokens    = tokenList
				end
				rawset(nodeCall, "first_line", getFirstLine(tokenList))
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:Is('String') then
				--string call
				local args = haxe.array.new()
				args:push(haxe.expr.StringExpr(tok:Get(tokenList).Constant))
				local nodeCall = haxe.expr.CallExpr(prim, args)
				if not options.disableEmitTokenList then
					nodeCall.tokens     = tokenList
				end
				rawset(nodeCall, "first_line", getFirstLine(tokenList))
				--
				prim = nodeCall

			elseif not onlyDotColon and tok:IsSymbol('{') then
				--table call
				local st, arg = ParseSimpleExpr()
				-- TODO: ParseExpr() parses the table AND and any following binary expressions.
				-- We just want the table
				if not st then return false, arg end

				local args = haxe.array.new()
				args:push(arg)
				local nodeCall = haxe.expr.CallExpr(prim, args)
				if not options.disableEmitTokenList then
					nodeCall.tokens    = tokenList
				end
				rawset(nodeCall, "first_line", prim.first_line)
				--
				prim = nodeCall

			else
				break
			end
		end
		return true, prim
	end


	function ParseSimpleExpr()
		local tokenList = {}

		if tok:Is('Number') then
			local nodeNum = haxe.expr.NumberExpr(tonumber(tok:Get(tokenList).Data))
			if not options.disableEmitTokenList then
				nodeNum.tokens  = tokenList
			end
			rawset(nodeNum, "first_line", getFirstLine(tokenList))
			return true, nodeNum

		elseif tok:Is('String') then
			local nodeStr = haxe.expr.StringExpr(tok:Get(tokenList).Constant)
			if not options.disableEmitTokenList then
				nodeStr.tokens  = tokenList
			end
			rawset(nodeStr, "first_line", getFirstLine(tokenList))
			return true, nodeStr

		elseif tok:ConsumeKeyword('nil', tokenList) then
			local nodeNil = haxe.expr.NilExpr
			if not options.disableEmitTokenList then
				nodeNil.tokens  = tokenList
			end
			rawset(nodeNil, "first_line", getFirstLine(tokenList))
			return true, nodeNil

		elseif tok:IsKeyword('false') or tok:IsKeyword('true') then
			local nodeBoolean = haxe.expr.BooleanExpr(tok:Get(tokenList).Data == 'true')
			if not options.disableEmitTokenList then
				nodeBoolean.tokens  = tokenList
			end
			rawset(nodeBoolean, "first_line", getFirstLine(tokenList))
			return true, nodeBoolean

		elseif tok:ConsumeSymbol('...', tokenList) then
			local nodeDots = haxe.expr.DotsExpr
			if not options.disableEmitTokenList then
				nodeDots.tokens   = tokenList
			end
			rawset(nodeDots, "first_line", getFirstLine(tokenList))
			return true, nodeDots

		elseif tok:ConsumeSymbol('{', tokenList) then
			local entry_list = haxe.array.new()
			--
			while true do
				if tok:IsSymbol('[') then
					--key
					tok:Get(tokenList)
					local st, key = ParseExpr()
					if not st then
						return false, GenerateError("Key expr Expected")
					end
					if not tok:ConsumeSymbol(']', tokenList) then
						return false, GenerateError("`]` Expected")
					end
					if not tok:ConsumeSymbol('=', tokenList) then
						return false, GenerateError("`=` Expected")
					end
					local st, value = ParseExpr()
					if not st then
						return false, GenerateError("value expr Expected")
					end
					entry_list:push(haxe.tableElem.Key(key, value))
				elseif tok:Is('Ident') then
					--value or key
					local lookahead = tok:Peek(1)
					if lookahead.__tag == 'Symbol' and lookahead.Data == '=' then
						--we are a key
						local key = tok:Get(tokenList)
						if not tok:ConsumeSymbol('=', tokenList) then
							return false, GenerateError("`=` Expected")
						end
						local st, value = ParseExpr()
						if not st then
							return false, GenerateError("value expr Expected")
						end
						entry_list:push(haxe.tableElem.KeyString(key.Data, value))
					else
						--we are a value
						local st, value = ParseExpr()
						if not st then
							return false, GenerateError("value Exected")
						end
						entry_list:push(haxe.tableElem.Value(value))
					end
				elseif tok:ConsumeSymbol('}', tokenList) then
					break

				else
					--value
					local st, value = ParseExpr()
					entry_list:push(haxe.tableElem.Value(value))
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
			local constructor = haxe.expr.ConstructorExpr(entry_list)
			if not options.disableEmitTokenList then
				constructor.tokens  = tokenList
			end
			rawset(constructor, "first_line", getFirstLine(tokenList))
			return true, constructor

		elseif tok:ConsumeKeyword('function', tokenList) then
			local st, func = ParseFunctionArgsAndBody(tokenList)
			if not st then return false, func end
			--
			local funcExpr = haxe.expr.FunctionExpr(func)
			rawset(funcExpr, "first_line", getFirstLine(tokenList))
			return true, funcExpr

		else
			return ParseSuffixedExpr(false)
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
	function ParseSubExpr(level)
		--base item, possibly with unop prefix
		local st, exp
		if unops[tok:Peek().Data] then
			local tokenList = {}
			local op = tok:Get(tokenList).Data
			st, exp = ParseSubExpr(unopprio)
			if not st then return false, exp end
			local nodeEx = haxe.expr.UnopExpr(exp, op, unopprio)
			if not options.disableEmitTokenList then
				nodeEx.tokens  = tokenList
			end
			rawset(nodeEx, "first_line", getFirstLine(tokenList))
			exp = nodeEx
		else
			st, exp = ParseSimpleExpr()
			if not st then return false, exp end
		end

		--next items in chain
		while true do
			local prio = priority[tok:Peek().Data]
			if prio and prio[1] > level then
				local tokenList = {}
				local op = tok:Get(tokenList).Data
				local st, rhs = ParseSubExpr(prio[2])
				if not st then return false, rhs end
				local nodeEx = haxe.expr.BinopExpr(exp, op, prio[1], rhs)
				if not options.disableEmitTokenList then
					nodeEx.tokens  = tokenList
				end
				rawset(nodeEx, "first_line", getFirstLine(tokenList))
				--
				exp = nodeEx
			else
				break
			end
		end

		return true, exp
	end


	ParseExpr = function()
		return ParseSubExpr(0)
	end


	local function ParseStatement()
		local stat = nil
		local tokenList = {}
		if tok:ConsumeKeyword('if', tokenList) then
			local clauses = {}

			--clauses
			repeat
				local st, nodeCond = ParseExpr()
				if not st then return false, nodeCond end
				if not tok:ConsumeKeyword('then', tokenList) then
					return false, GenerateError("`then` expected.")
				end
				local st, nodeBody = ParseStatementList()
				if not st then return false, nodeBody end
				clauses[#clauses+1] = {
					cond = nodeCond,
					body = nodeBody,
				}
			until not tok:ConsumeKeyword('elseif', tokenList)

			local elseBody
			--else clause
			if tok:ConsumeKeyword('else', tokenList) then
				local st, nodeBody = ParseStatementList()
				if not st then return false, nodeBody end
				elseBody = nodeBody
			else
				elseBody = haxe.array.new()
			end

			-- construct a chain of if statements backwards going from the last else
			local prevElseBody = elseBody
			local nodeIfStat
			for i = #clauses, 1, -1 do
				nodeIfStat = haxe.stmt.IfStatement(clauses[i].cond, clauses[i].body, prevElseBody)
				prevElseBody = haxe.array.new()
				prevElseBody:push(nodeIfStat)
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
			--condition
			local st, nodeCond = ParseExpr()
			if not st then return false, nodeCond end

			--do
			if not tok:ConsumeKeyword('do', tokenList) then
				return false, GenerateError("`do` expected.")
			end

			--body
			local st, nodeBody = ParseStatementList()
			if not st then return false, nodeBody end

			--end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			--return
			local nodeWhileStat = haxe.stmt.WhileStatement(nodeCond, nodeBody)
			if not options.disableEmitTokenList then
				nodeWhileStat.tokens    = tokenList
			end
			stat = nodeWhileStat

		elseif tok:ConsumeKeyword('do', tokenList) then
			--do block
			local st, nodeBlock = ParseStatementList()
			if not st then return false, nodeBlock end
			if not tok:ConsumeKeyword('end', tokenList) then
				return false, GenerateError("`end` expected.")
			end

			local nodeDoStat = haxe.stmt.DoStatement(nodeBlock)
			if not options.disableEmitTokenList then
				nodeDoStat.tokens  = tokenList
			end
			rawset(nodeDoStat, "first_line", getFirstLine(tokenList))
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
				local st, startEx = ParseExpr()
				if not st then return false, startEx end
				if not tok:ConsumeSymbol(',', tokenList) then
					return false, GenerateError("`,` Expected")
				end
				local st, endEx = ParseExpr()
				if not st then return false, endEx end
				local st, stepEx;
				if tok:ConsumeSymbol(',', tokenList) then
					st, stepEx = ParseExpr()
					if not st then return false, stepEx end
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected")
				end

				local st, body = ParseStatementList()
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected")
				end
				flushScope(savedp)
				--
				if stepEx then
					stepEx = haxe.option.Some(stepEx)
				else
					stepEx = haxe.option.None
				end
				local nodeFor = haxe.stmt.NumericForStatement(forVar, startEx, endEx, stepEx, body)
				if not options.disableEmitTokenList then
					nodeFor.tokens   = tokenList
				end
				rawset(nodeFor, "first_line", getFirstLine(tokenList))
				stat = nodeFor
			else
				--generic for
				--
				local savedp = #curScopeVars
				local varList = haxe.array.new()
				varList:push(baseVarName.Data)
				addVar(baseVarName.Data)

				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("for variable expected.")
					end
					local var = tok:Get(tokenList).Data
					varList:push(var)
					addVar(var)
				end
				if not tok:ConsumeKeyword('in', tokenList) then
					return false, GenerateError("`in` expected.")
				end
				local generators = haxe.array.new()
				local st, firstGenerator = ParseExpr()
				if not st then return false, firstGenerator end
				generators:push(firstGenerator)
				while tok:ConsumeSymbol(',', tokenList) do
					local st, gen = ParseExpr()
					if not st then return false, gen end
					generators:push(gen)
				end
				if not tok:ConsumeKeyword('do', tokenList) then
					return false, GenerateError("`do` expected.")
				end

				local st, body = ParseStatementList()
				if not st then return false, body end
				if not tok:ConsumeKeyword('end', tokenList) then
					return false, GenerateError("`end` expected.")
				end
				flushScope(savedp)
				--
				local nodeFor = haxe.stmt.GenericForStatement(varList, generators, body)
				if not options.disableEmitTokenList then
					nodeFor.tokens       = tokenList
				end
				rawset(nodeFor, "first_line", getFirstLine(tokenList))
				stat = nodeFor
			end

		elseif tok:ConsumeKeyword('repeat', tokenList) then
			local st, body = ParseStatementList()
			if not st then return false, body end
			--
			if not tok:ConsumeKeyword('until', tokenList) then
				return false, GenerateError("`until` expected.")
			end
			-- FIX: Used to parse in parent scope
			-- Now parses in repeat scope
			local st, cond = ParseExpr()
			if not st then return false, cond end
			--
			local nodeRepeat = haxe.stmt.RepeatStatement(cond, body)
			if not options.disableEmitTokenList then
				nodeRepeat.tokens    = tokenList
			end
			rawset(nodeRepeat, "first_line", getFirstLine(tokenList))
			stat = nodeRepeat

		elseif tok:ConsumeKeyword('function', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("Function name expected")
			end
			local st, name = ParseSuffixedExpr(true) --true => only dots and colons
			if not st then return false, name end
			--
			local st, func = ParseFunctionArgsAndBody(tokenList)
			if not st then return false, func end
			--
			stat = haxe.stmt.FunctionStatement(name, false, func)
			rawset(stat, "first_line", getFirstLine(tokenList))

		elseif tok:ConsumeKeyword('local', tokenList) then
			if tok:Is('Ident') then
				local varList = haxe.array.new()
				varList:push(tok:Get(tokenList).Data)
				while tok:ConsumeSymbol(',', tokenList) do
					if not tok:Is('Ident') then
						return false, GenerateError("local var name expected")
					end
					varList:push(tok:Get(tokenList).Data)
				end

				local initList = haxe.array.new()
				if tok:ConsumeSymbol('=', tokenList) then
					repeat
						local st, ex = ParseExpr()
						if not st then return false, ex end
						initList:push(ex)
					until not tok:ConsumeSymbol(',', tokenList)
				end

				local nodeLocal = haxe.stmt.LocalStatement(varList, initList)
				if not options.disableEmitTokenList then
					nodeLocal.tokens    = tokenList
				end
				rawset(nodeLocal, "first_line", getFirstLine(tokenList))
				--
				stat = nodeLocal

			elseif tok:ConsumeKeyword('function', tokenList) then
				if not tok:Is('Ident') then
					return false, GenerateError("Function name expected")
				end
				local savedp = #curScopeVars
				local varName = tok:Get(tokenList).Data
				local name = haxe.expr.VarExpr(varName)
				addVar(varName)
				--
				local st, func = ParseFunctionArgsAndBody(tokenList)
				if not st then return false, func end
				flushScope(savedp)
				-- 
				stat = haxe.stmt.FunctionStatement(name, true, func)
				rawset(stat, "first_line", getFirstLine(tokenList))

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
			local nodeLabel = haxe.stmt.LabelStatement(label)
			if not options.disableEmitTokenList then
				nodeLabel.tokens  = tokenList
			end
			rawset(nodeLabel, "first_line", getFirstLine(tokenList))
			stat = nodeLabel

		elseif tok:ConsumeKeyword('return', tokenList) then
			local exList = haxe.array.new()
			if not tok:IsKeyword('end') then
				local st, firstEx = ParseExpr()
				if st then
					exList:push(firstEx)
					while tok:ConsumeSymbol(',', tokenList) do
						local st, ex = ParseExpr()
						if not st then return false, ex end
						exList:push(ex)
					end
				end
			end

			local nodeReturn = haxe.stmt.ReturnStatement(exList)
			if not options.disableEmitTokenList then
				nodeReturn.tokens    = tokenList
			end
			rawset(nodeReturn, "first_line", getFirstLine(tokenList))
			stat = nodeReturn

		elseif tok:ConsumeKeyword('break', tokenList) then
			local nodeBreak = haxe.stmt.BreakStatement
			if not options.disableEmitTokenList then
				nodeBreak.tokens  = tokenList
			end
			rawset(nodeBreak, "first_line", getFirstLine(tokenList))
			stat = nodeBreak

		elseif tok:ConsumeKeyword('goto', tokenList) then
			if not tok:Is('Ident') then
				return false, GenerateError("label expected")
			end
			local label = tok:Get(tokenList).Data
			local nodeGoto = haxe.stmt.GotoStatement(label)
			if not options.disableEmitTokenList then
				nodeGoto.tokens  = tokenList
			end
			rawset(nodeGoto, "first_line", getFirstLine(tokenList))
			stat = nodeGoto

		else
			--statementParseExpr
			local st, suffixed = ParseSuffixedExpr(false)
			if not st then return false, suffixed end

			--assignment or call?
			if tok:IsSymbol(',') or tok:IsSymbol('=') then
				--check that it was not parenthesized, making it not an lvalue
				if suffixed.parenthesized then
					return false, GenerateError("Can not assign to parenthesized expression, is not an lvalue")
				end

				--more processing needed
				local lhs = haxe.array.new()
				lhs:push(suffixed)
				while tok:ConsumeSymbol(',', tokenList) do
					local st, lhsPart = ParseSuffixedExpr(false)
					if not st then return false, lhsPart end
					lhs:push(lhsPart)
				end

				--equals
				if not tok:ConsumeSymbol('=', tokenList) then
					return false, GenerateError("`=` Expected.")
				end

				--rhs
				local rhs = haxe.array.new()
				local st, firstRhs = ParseExpr()
				if not st then return false, firstRhs end
				rhs:push(firstRhs)
				while tok:ConsumeSymbol(',', tokenList) do
					local st, rhsPart = ParseExpr()
					if not st then return false, rhsPart end
					rhs:push(rhsPart)
				end

				--done
				local nodeAssign = haxe.stmt.AssignmentStatement(lhs, rhs)
				if not options.disableEmitTokenList then
					nodeAssign.tokens  = tokenList
				end
				rawset(nodeAssign, "first_line", getFirstLine(tokenList))
				stat = nodeAssign

			elseif suffixed[0] == 'CallExpr' then
				--it's a call statement
				local nodeCall = haxe.stmt.CallStatement(suffixed[2], suffixed[3]) -- base, args
				if not options.disableEmitTokenList then
					nodeCall.tokens     = tokenList
				end
				rawset(nodeCall, "first_line", suffixed[2].first_line)
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
		return true, stat
	end


	local statListCloseKeywords = lookupify{'end', 'else', 'elseif', 'until'}

	ParseStatementList = function()
		local nodeStatlist = haxe.array.new()

		local st, nodeStatement
		local savedp = #curScopeVars
		local isFirst = true
		while not statListCloseKeywords[tok:Peek().Data] and not tok:IsEof() do
			st, nodeStatement = ParseStatement()
			if not st then return false, nodeStatement end
 
			local isLast = statListCloseKeywords[tok:Peek().Data] or tok:IsEof()
			for _, stmt in ipairs(hooks.statement(nodeStatement, curScopeVars, savedp, isFirst, isLast)) do
				nodeStatlist:push(stmt)
			end
			isFirst = false

			-- variables decleared as local aren't in scope when defining them
			-- so they should be added to scope only after they're fully defined
			if nodeStatement[0] == "LocalStatement" then
				local names = nodeStatement[2]
				for i = 1, #names do
					addVar(names[i])
				end
			end
		end

		-- remove variables when leaving scope
		flushScope(savedp)

		return true, nodeStatlist
	end


	local function mainfunc()
		return ParseStatementList()
	end

	local st, main = mainfunc()
	return main
end

return Parser
	
