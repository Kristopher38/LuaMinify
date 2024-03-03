require'strict'
require'ParseLua'
local util = require'Util'

local function debug_printf(...)
	--[[
	util.printf(...)
	--]]
end

--
-- FormatIdentity.lua
--
-- Returns the exact source code that was used to create an AST, preserving all
-- comments and whitespace.
-- This can be used to get back a Lua source after renaming some variables in
-- an AST.
--

local function Format_Identity(ast)
	local out = {
		rope = {},  -- List of strings
		line = 1,
		char = 1,

		appendStr = function(self, str)
			table.insert(self.rope, str)

			local lines = util.splitLines(str)
			if #lines == 1 then
				self.char = self.char + #str
			else
				self.line = self.line + #lines - 1
				local lastLine = lines[#lines]
				self.char = #lastLine
			end
		end,

		appendToken = function(self, token)
			self:appendWhite(token)
			--[*[
			--debug_printf("appendToken(%q)", token.Data)
			local data  = token.Data
			local lines = util.splitLines(data)
			while self.line + #lines < token.Line do
				print("Inserting extra line")
				self.str  = self.str .. '\n'
				self.line = self.line + 1
				self.char = 1
			end
			--]]
			self:appendStr(token.Data)
		end,

		appendTokens = function(self, tokens)
			for _,token in ipairs(tokens) do
				self:appendToken( token )
			end
		end,

		appendWhite = function(self, token)
			if token.LeadingWhite then
				self:appendTokens( token.LeadingWhite )
				--self.str = self.str .. ' '
			end
		end
	}

	local formatStatlist, formatExpr;

	formatExpr = function(expr)
		local tok_it = 1
		local function appendNextToken(str)
			local tok = expr.tokens[tok_it];
			if str and tok.Data ~= str then
				error("Expected token '" .. str .. "'. tokens: " .. util.PrintTable(expr.tokens))
			end
			out:appendToken( tok )
			tok_it = tok_it + 1
		end
		local function appendToken(token)
			out:appendToken( token )
			tok_it = tok_it + 1
		end
		local function appendWhite()
			local tok = expr.tokens[tok_it];
			if not tok then error(util.PrintTable(expr)) end
			out:appendWhite( tok )
			tok_it = tok_it + 1
		end
		local function appendStr(str)
			appendWhite()
			out:appendStr(str)
		end
		local function peek()
			if tok_it < #expr.tokens then
				return expr.tokens[tok_it].Data
			end
		end
		local function appendComma(mandatory, seperators)
			if true then
				seperators = seperators or { "," }
				seperators = util.lookupify( seperators )
				if not mandatory and not seperators[peek()] then
					return
				end
				assert(seperators[peek()], "Missing comma or semicolon")
				appendNextToken()
			else
				local p = peek()
				if p == "," or p == ";" then
					appendNextToken()
				end
			end
		end

		debug_printf("formatExpr(%s) at line %i", expr.__tag, expr.tokens[1] and expr.tokens[1].Line or -1)

		if expr.__tag == 'VarExpr' then
			if expr.var then
				appendStr( expr.var.name )
			else
				appendStr( expr.name )
			end

		elseif expr.__tag == 'NumberExpr' then
			appendToken( expr.value )

		elseif expr.__tag == 'StringExpr' then
			appendToken( expr.value )

		elseif expr.__tag == 'BooleanExpr' then
			appendNextToken( expr.value and "true" or "false" )

		elseif expr.__tag == 'NilExpr' then
			appendNextToken( "nil" )

		elseif expr.__tag == 'BinopExpr' then
			formatExpr(expr.lhs)
			appendStr( expr.op )
			formatExpr(expr.rhs)

		elseif expr.__tag == 'UnopExpr' then
			appendStr( expr.op )
			formatExpr(expr.rhs)

		elseif expr.__tag == 'DotsExpr' then
			appendNextToken( "..." )

		elseif expr.__tag == 'CallExpr' then
			formatExpr(expr.base)
			appendNextToken( "(" )
			for i,arg in ipairs( expr.args ) do
				formatExpr(arg)
				appendComma( i ~= #expr.args )
			end
			appendNextToken( ")" )

		elseif expr.__tag == 'TableCallExpr' then
			formatExpr( expr.base )
			formatExpr( expr.arg )

		elseif expr.__tag == 'StringCallExpr' then
			formatExpr(expr.base)
			appendToken( expr.arg )

		elseif expr.__tag == 'IndexExpr' then
			formatExpr(expr.base)
			appendNextToken( "[" )
			formatExpr(expr.index)
			appendNextToken( "]" )

		elseif expr.__tag == 'MemberExpr' then
			formatExpr(expr.base)
			appendNextToken()  -- . or :
			appendToken(expr.ident)

		elseif expr.__tag == 'FunctionExpr' then
			-- anonymous function
			appendNextToken( "function" )
			appendNextToken( "(" )
			if #expr.args > 0 then
				for i = 1, #expr.args do
					appendStr( expr.args[i].name )
					if i ~= #expr.args then
						appendNextToken(",")
					elseif expr.vararg then
						appendNextToken(",")
						appendNextToken("...")
					end
				end
			elseif expr.vararg then
				appendNextToken("...")
			end
			appendNextToken(")")
			formatStatlist(expr.body)
			appendNextToken("end")

		elseif expr.__tag == 'ConstructorExpr' then
			appendNextToken( "{" )
			for i = 1, #expr.entry_list do
				local entry = expr.entry_list[i]
				if entry.__tag == 'Key' then
					appendNextToken( "[" )
					formatExpr(entry.key)
					appendNextToken( "]" )
					appendNextToken( "=" )
					formatExpr(entry.value)
				elseif entry.__tag == 'Value' then
					formatExpr(entry.value)
				elseif entry.__tag == 'KeyString' then
					appendStr(entry.key)
					appendNextToken( "=" )
					formatExpr(entry.value)
				end
				appendComma( i ~= #expr.entry_list, { ",", ";" } )
			end
			appendNextToken( "}" )

		elseif expr.__tag == 'ParenExpr' then
			appendNextToken( "(" )
			formatExpr(expr.inner)
			appendNextToken( ")" )

		else
			print("Unknown AST __tag: ", statement.__tag)
		end

		assert(tok_it == #expr.tokens + 1)
		debug_printf("/formatExpr")
	end


	local formatStatement = function(statement)
		local tok_it = 1
		local function appendNextToken(str)
			local tok = statement.tokens[tok_it];
			assert(tok, string.format("Not enough tokens for %q. First token at %i:%i",
				str, statement.tokens[1].Line, statement.tokens[1].Char))
			assert(tok.Data == str,
				string.format('Expected token %q, got %q', str, tok.Data))
			out:appendToken( tok )
			tok_it = tok_it + 1
		end
		local function appendToken(token)
			out:appendToken( str )
			tok_it = tok_it + 1
		end
		local function appendWhite()
			local tok = statement.tokens[tok_it];
			out:appendWhite( tok )
			tok_it = tok_it + 1
		end
		local function appendStr(str)
			appendWhite()
			out:appendStr(str)
		end
		local function appendComma(mandatory)
			if mandatory
			   or (tok_it < #statement.tokens and statement.tokens[tok_it].Data == ",") then
			   appendNextToken( "," )
			end
		end

		debug_printf("")
		debug_printf(string.format("formatStatement(%s) at line %i", statement.__tag, statement.tokens[1] and statement.tokens[1].Line or -1))

		if statement.__tag == 'AssignmentStatement' then
			for i,v in ipairs(statement.lhs) do
				formatExpr(v)
				appendComma( i ~= #statement.lhs )
			end
			if #statement.rhs > 0 then
				appendNextToken( "=" )
				for i,v in ipairs(statement.rhs) do
					formatExpr(v)
					appendComma( i ~= #statement.rhs )
				end
			end

		elseif statement.__tag == 'CallStatement' then
			formatExpr(statement)

		elseif statement.__tag == 'LocalStatement' then
			appendNextToken( "local" )
			for i = 1, #statement.names do
				appendStr( statement.names[i].name )
				appendComma( i ~= #statement.names )
			end
			if #statement.init_exprs > 0 then
				appendNextToken( "=" )
				for i = 1, #statement.init_exprs do
					formatExpr(statement.init_exprs[i])
					appendComma( i ~= #statement.init_exprs )
				end
			end

		elseif statement.__tag == 'IfStatement' then
			appendNextToken( "if" )
			formatExpr( statement.clauses[1].cond )
			appendNextToken( "then" )
			formatStatlist( statement.clauses[1].body )
			for i = 2, #statement.clauses do
				local st = statement.clauses[i]
				if st.cond then
					appendNextToken( "elseif" )
					formatExpr(st.cond)
					appendNextToken( "then" )
				else
					appendNextToken( "else" )
				end
				formatStatlist(st.body)
			end
			appendNextToken( "end" )

		elseif statement.__tag == 'WhileStatement' then
			appendNextToken( "while" )
			formatExpr(statement.cond)
			appendNextToken( "do" )
			formatStatlist(statement.body)
			appendNextToken( "end" )

		elseif statement.__tag == 'DoStatement' then
			appendNextToken( "do" )
			formatStatlist(statement.body)
			appendNextToken( "end" )

		elseif statement.__tag == 'ReturnStatement' then
			appendNextToken( "return" )
			for i = 1, #statement.args do
				formatExpr(statement.args[i])
				appendComma( i ~= #statement.args )
			end

		elseif statement.__tag == 'BreakStatement' then
			appendNextToken( "break" )

		elseif statement.__tag == 'RepeatStatement' then
			appendNextToken( "repeat" )
			formatStatlist(statement.body)
			appendNextToken( "until" )
			formatExpr(statement.cond)

		elseif statement.__tag == 'FunctionStatement' then
			--print(util.PrintTable(statement))

			if statement.is_local then
				appendNextToken( "local" )
			end
			appendNextToken( "function" )

			if statement.is_local then
				appendStr(statement.name.name)
			else
				formatExpr(statement.name)
			end

			appendNextToken( "(" )
			if #statement.args > 0 then
				for i = 1, #statement.args do
					appendStr( statement.args[i].name )
					appendComma( i ~= #statement.args or statement.vararg )
					if i == #statement.args and statement.vararg then
						appendNextToken( "..." )
					end
				end
			elseif statement.vararg then
				appendNextToken( "..." )
			end
			appendNextToken( ")" )

			formatStatlist(statement.body)
			appendNextToken( "end" )

		elseif statement.__tag == 'GenericForStatement' then
			appendNextToken( "for" )
			for i = 1, #statement.var_list do
				appendStr( statement.var_list[i].name )
				appendComma( i ~= #statement.var_list )
			end
			appendNextToken( "in" )
			for i = 1, #statement.generators do
				formatExpr(statement.generators[i])
				appendComma( i ~= #statement.generators )
			end
			appendNextToken( "do" )
			formatStatlist(statement.body)
			appendNextToken( "end" )

		elseif statement.__tag == 'NumericForStatement' then
			appendNextToken( "for" )
			appendStr( statement.var.name )
			appendNextToken( "=" )
			formatExpr(statement.start)
			appendNextToken( "," )
			formatExpr(statement.finish)
			if statement.step then
				appendNextToken( "," )
				formatExpr(statement.step)
			end
			appendNextToken( "do" )
			formatStatlist(statement.body)
			appendNextToken( "end" )

		elseif statement.__tag == 'LabelStatement' then
			appendNextToken( "::" )
			appendStr( statement.label )
			appendNextToken( "::" )

		elseif statement.__tag == 'GotoStatement' then
			appendNextToken( "goto" )
			appendStr( statement.label )

		elseif statement.__tag == 'Eof' then
			appendWhite()

		else
			print("Unknown AST __tag: ", statement.__tag)
		end

		if statement.Semicolon then
			appendNextToken(";")
		end

		assert(tok_it == #statement.tokens + 1)
		debug_printf("/formatStatment")
	end

	formatStatlist = function(statList)
		for _, stat in ipairs(statList.body) do
			formatStatement(stat)
		end
	end

	formatStatlist({body = ast})
	
	return true, table.concat(out.rope)
end

return Format_Identity
