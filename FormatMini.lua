
local parser = require'ParseLua'
local ParseLua = parser
local util = require'Util'
local lookupify = util.lookupify

--
-- FormatMini.lua
--
-- Returns the minified version of an AST. Operations which are performed:
-- - All comments and whitespace are ignored
-- - All local variables are renamed
--

local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
local Symbols = lookupify{'+', '-', '*', '/', '^', '%', ',', '{', '}', '[', ']', '(', ')', ';', '#'}

local function Format_Mini(ast)
	local formatStatlist, formatExpr;
	--
	local function joinStatementsSafe(a, b, sep)
		sep = sep or ' '
		local aa, bb = a:sub(-1,-1), b:sub(1,1)
		if UpperChars[aa] or LowerChars[aa] or aa == '_' then
			if not (UpperChars[bb] or LowerChars[bb] or bb == '_' or Digits[bb]) then
				--bb is a symbol, can join without sep
				return a..b
			elseif bb == '(' then
				print("==============>>>",aa,bb)
				--prevent ambiguous syntax
				return a..sep..b
			else
				return a..sep..b
			end
		elseif Digits[aa] then
			if bb == '(' then
				--can join statements directly
				return a..b
			elseif Symbols[bb] then
				return a .. b
			else
				return a..sep..b
			end
		elseif aa == '' then
			return a..b
		else
			if bb == '(' then
				--don't want to accidentally call last statement, can't join directly
				return a..sep..b
			else
			--print("asdf", '"'..a..'"', '"'..b..'"')
				return a..b
			end
		end
	end

	formatExpr = function(expr, precedence)
		local precedence = precedence or 0
		local currentPrecedence = 0
		local skipParens = false
		local out = ""
		if expr.__tag == 'VarExpr' then
			out = out..expr.name

		elseif expr.__tag == 'NumberExpr' then
			out = out..expr.value

		elseif expr.__tag == 'StringExpr' then
			out = out..expr.value

		elseif expr.__tag == 'BooleanExpr' then
			out = out..tostring(expr.value)

		elseif expr.__tag == 'NilExpr' then
			out = joinStatementsSafe(out, "nil")

		elseif expr.__tag == 'BinopExpr' then
			currentPrecedence = expr.op_prec
			out = joinStatementsSafe(out, formatExpr(expr.lhs, currentPrecedence))
			out = joinStatementsSafe(out, expr.op)
			out = joinStatementsSafe(out, formatExpr(expr.rhs))
			if expr.op == '^' or expr.op == '..' then
				currentPrecedence = currentPrecedence - 1
			end
			
			if currentPrecedence < precedence then
				skipParens = false
			else
				skipParens = true
			end
			--print(skipParens, precedence, currentPrecedence)
		elseif expr.__tag == 'UnopExpr' then
			out = joinStatementsSafe(out, expr.op)
			out = joinStatementsSafe(out, formatExpr(expr.rhs))

		elseif expr.__tag == 'DotsExpr' then
			out = out.."..."

		elseif expr.__tag == 'CallExpr' or expr.__tag == 'CallStatement' then
			out = out..formatExpr(expr.base)
			out = out.."("
			for i = 1, #expr.args do
				out = out..formatExpr(expr.args[i])
				if i ~= #expr.args then
					out = out..","
				end
			end
			out = out..")"

		elseif expr.__tag == 'TableCallExpr' then
			out = out..formatExpr(expr.base)
			out = out..formatExpr(expr.arg)

		elseif expr.__tag == 'StringCallExpr' then
			out = out..formatExpr(expr.base)
			out = out..expr.arg

		elseif expr.__tag == 'IndexExpr' then
			out = out..formatExpr(expr.base).."["..formatExpr(expr.index).."]"

		elseif expr.__tag == 'MemberExpr' then
			out = out..formatExpr(expr.base)..expr.indexer..expr.ident

		elseif expr.__tag == 'FunctionExpr' then
			out = out.."function("
			if #expr.args > 0 then
				for i = 1, #expr.args do
					out = out..expr.args[i]
					if i ~= #expr.args then
						out = out..","
					elseif expr.vararg then
						out = out..",..."
					end
				end
			elseif expr.vararg then
				out = out.."..."
			end
			out = out..")"
			out = joinStatementsSafe(out, formatStatlist(expr.body))
			out = joinStatementsSafe(out, "end")

		elseif expr.__tag == 'ConstructorExpr' then
			out = out.."{"
			for i = 1, #expr.entry_list do
				local entry = expr.entry_list[i]
				if entry.__tag == 'Key' then
					out = out.."["..formatExpr(entry.key).."]="..formatExpr(entry.value)
				elseif entry.__tag == 'Value' then
					out = out..formatExpr(entry.value)
				elseif entry.__tag == 'KeyString' then
					out = out..entry.key.."="..formatExpr(entry.value)
				end
				if i ~= #expr.entry_list then
					out = out..","
				end
			end
			out = out.."}"

		elseif expr.__tag == 'ParenExpr' then
			out = out.."("..formatExpr(expr.inner)..")"

		end
		return --[[print(out) or]] out
	end

	local formatStatement = function(statement)
		local out = ''
		if statement.__tag == 'VerbatimCode' then
			out = statement.Data
		elseif statement.__tag == 'AssignmentStatement' then
			for i = 1, #statement.lhs do
				out = out..formatExpr(statement.lhs[i])
				if i ~= #statement.lhs then
					out = out..","
				end
			end
			if #statement.rhs > 0 then
				out = out.."="
				for i = 1, #statement.rhs do
					out = out..formatExpr(statement.rhs[i])
					if i ~= #statement.rhs then
						out = out..","
					end
				end
			end

		elseif statement.__tag == 'CallStatement' then
			out = formatExpr(statement)

		elseif statement.__tag == 'LocalStatement' then
			out = out.."local "
			for i = 1, #statement.names do
				out = out..statement.names[i]
				if i ~= #statement.names then
					out = out..","
				end
			end
			if #statement.init_exprs > 0 then
				out = out.."="
				for i = 1, #statement.init_exprs do
					out = out..formatExpr(statement.init_exprs[i])
					if i ~= #statement.init_exprs then
						out = out..","
					end
				end
			end

		elseif statement.__tag == 'IfStatement' then
			out = joinStatementsSafe("if", formatExpr(statement.clauses[1].cond))
			out = joinStatementsSafe(out, "then")
			out = joinStatementsSafe(out, formatStatlist(statement.clauses[1].body))
			for i = 2, #statement.clauses do
				local st = statement.clauses[i]
				if st.cond then
					out = joinStatementsSafe(out, "elseif")
					out = joinStatementsSafe(out, formatExpr(st.cond))
					out = joinStatementsSafe(out, "then")
				else
					out = joinStatementsSafe(out, "else")
				end
				out = joinStatementsSafe(out, formatStatlist(st.body))
			end
			out = joinStatementsSafe(out, "end")

		elseif statement.__tag == 'WhileStatement' then
			out = joinStatementsSafe("while", formatExpr(statement.cond))
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.body))
			out = joinStatementsSafe(out, "end")

		elseif statement.__tag == 'DoStatement' then
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.body))
			out = joinStatementsSafe(out, "end")

		elseif statement.__tag == 'ReturnStatement' then
			out = "return"
			for i = 1, #statement.args do
				out = joinStatementsSafe(out, formatExpr(statement.args[i]))
				if i ~= #statement.args then
					out = out..","
				end
			end

		elseif statement.__tag == 'BreakStatement' then
			out = "break"

		elseif statement.__tag == 'RepeatStatement' then
			out = "repeat"
			out = joinStatementsSafe(out, formatStatlist(statement.body))
			out = joinStatementsSafe(out, "until")
			out = joinStatementsSafe(out, formatExpr(statement.cond))

		elseif statement.__tag == 'FunctionStatement' then
			if statement.is_local then
				out = "local"
			end
			out = joinStatementsSafe(out, "function ")
			if statement.is_local then
				out = out..statement.name
			else
				out = out..formatExpr(statement.name)
			end
			out = out.."("
			if #statement.f.args > 0 then
				for i = 1, #statement.f.args do
					out = out..statement.f.args[i]
					if i ~= #statement.f.args then
						out = out..","
					elseif statement.f.vararg then
						--print("Apply vararg")
						out = out..",..."
					end
				end
			elseif statement.f.vararg then
				out = out.."..."
			end
			out = out..")"
			out = joinStatementsSafe(out, formatStatlist(statement.f.body))
			out = joinStatementsSafe(out, "end")

		elseif statement.__tag == 'GenericForStatement' then
			out = "for "
			for i = 1, #statement.var_list do
				out = out..statement.var_list[i]
				if i ~= #statement.var_list then
					out = out..","
				end
			end
			out = out.." in"
			for i = 1, #statement.generators do
				out = joinStatementsSafe(out, formatExpr(statement.generators[i]))
				if i ~= #statement.generators then
					out = joinStatementsSafe(out, ',')
				end
			end
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.body))
			out = joinStatementsSafe(out, "end")

		elseif statement.__tag == 'NumericForStatement' then
			out = "for "
			out = out..statement.var.."="
			out = out..formatExpr(statement.start)..","..formatExpr(statement.finish)
			if statement.step.__tag == "Some" then
				out = out..","..formatExpr(statement.step[1])
			end
			out = joinStatementsSafe(out, "do")
			out = joinStatementsSafe(out, formatStatlist(statement.body))
			out = joinStatementsSafe(out, "end")
		elseif statement.__tag == 'LabelStatement' then
			out = getIndentation() .. "::" .. statement.label .. "::"
		elseif statement.__tag == 'GotoStatement' then
			out = getIndentation() .. "goto " .. statement.label
		elseif statement.__tag == 'Comment' then
			-- ignore
		elseif statement.__tag == 'Eof' then
			-- ignore
		else
			print("Unknown AST __tag: " .. statement.__tag)
		end
		return out
	end

	formatStatlist = function(statList)
		local out = ''
		for _, stat in pairs(statList.body) do
			out = joinStatementsSafe(out, formatStatement(stat), ';')
		end
		return out
	end

	return formatStatlist({body = ast})
end

return Format_Mini
