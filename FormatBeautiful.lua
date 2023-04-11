--
-- Beautifier
--
-- Returns a beautified version of the code, including comments
--

local parser = require"ParseLua"
local ParseLua = parser
local util = require'Util'
local lookupify = util.lookupify

local LowerChars = lookupify{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 
							 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 
							 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}
local UpperChars = lookupify{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 
							 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 
							 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
local Digits = lookupify{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}

local function Format_Beautify(ast)
	local formatStatlist, formatExpr
	local indent = 0
	local EOL = "\n"
	local curLine = 1
	
	local function getIndentation(stmt)
		if stmt.AstType ~= "VerbatimCode" and curLine < stmt.FirstLine then
			local toCatchUp = stmt.FirstLine - curLine
			curLine = curLine + toCatchUp
			--return string.format("%s%d:    ", string.rep("\n", toCatchUp), curLine)
			return string.rep("\n", toCatchUp)
		else
			return " "
		end
	end
	
	local function joinStatementsSafe(a, b, sep)
		sep = sep or ''
		local aa, bb = a:sub(-1,-1), b:sub(1,1)
		if UpperChars[aa] or LowerChars[aa] or aa == '_' then
			if not (UpperChars[bb] or LowerChars[bb] or bb == '_' or Digits[bb]) then
				--bb is a symbol, can join without sep
				return a .. b
			elseif bb == '(' then
				--prevent ambiguous syntax
				return a..sep..b
			else
				return a..sep..b
			end
		elseif Digits[aa] then
			if bb == '(' then
				--can join statements directly
				return a..b
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
				return a..b
			end
		end
	end

	formatExpr = function(expr)
		local out = string.rep('(', expr.ParenCount or 0)
		if expr.AstType == 'VerbatimCode' then
			out = out .. expr.Data
		elseif expr.AstType == 'VarExpr' then
			out = out .. expr.Name

		elseif expr.AstType == 'NumberExpr' then
			out = out..expr.Value

		elseif expr.AstType == 'StringExpr' then
			out = out..expr.Value

		elseif expr.AstType == 'BooleanExpr' then
			out = out..tostring(expr.Value)

		elseif expr.AstType == 'NilExpr' then
			out = joinStatementsSafe(out, "nil")

		elseif expr.AstType == 'BinopExpr' then
			out = joinStatementsSafe(out, formatExpr(expr.Lhs)) .. " "
			out = joinStatementsSafe(out, expr.Op) .. " "
			out = joinStatementsSafe(out, formatExpr(expr.Rhs))

		elseif expr.AstType == 'UnopExpr' then
			out = joinStatementsSafe(out, expr.Op) .. (#expr.Op ~= 1 and " " or "")
			out = joinStatementsSafe(out, formatExpr(expr.Rhs))

		elseif expr.AstType == 'DotsExpr' then
			out = out.."..."

		elseif expr.AstType == 'CallExpr' then
			out = out..formatExpr(expr.Base)
			out = out.."("
			for i = 1, #expr.Arguments do
				out = out..formatExpr(expr.Arguments[i])
				if i ~= #expr.Arguments then
					out = out..", "
				end
			end
			out = out..")"

		elseif expr.AstType == 'TableCallExpr' then
			out = out..formatExpr(expr.Base) .. " "
			out = out..formatExpr(expr.Arguments[1])

		elseif expr.AstType == 'StringCallExpr' then
			out = out..formatExpr(expr.Base) .. " "
			out = out..expr.Arguments[1]

		elseif expr.AstType == 'IndexExpr' then
			out = out..formatExpr(expr.Base).."["..formatExpr(expr.Index).."]"

		elseif expr.AstType == 'MemberExpr' then
			out = out..formatExpr(expr.Base)..expr.Indexer..expr.Ident

		elseif expr.AstType == 'Function' then
			-- anonymous function
			out = out.."function("
			if #expr.Arguments > 0 then
				for i = 1, #expr.Arguments do
					out = out..expr.Arguments[i]
					if i ~= #expr.Arguments then
						out = out..", "
					elseif expr.VarArg then
						out = out..", ..."
					end
				end
			elseif expr.VarArg then
				out = out.."..."
			end
			out = out..")" .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(expr.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(expr) .. "end")
		elseif expr.AstType == 'ConstructorExpr' then
			out = out.."{ "
			for i = 1, #expr.EntryList do
				local entry = expr.EntryList[i]
				if entry.Type == 'Key' then
					out = out.."["..formatExpr(entry.Key).."] = "..formatExpr(entry.Value)
				elseif entry.Type == 'Value' then
					out = out..formatExpr(entry.Value)
				elseif entry.Type == 'KeyString' then
					out = out..entry.Key.." = "..formatExpr(entry.Value)
				end
				if i ~= #expr.EntryList then
					out = out..", "
				end
			end
			out = out.." }"

		elseif expr.AstType == 'Parentheses' then
			out = out.."("..formatExpr(expr.Inner)..")"

		end
		out = out..string.rep(')', expr.ParenCount or 0)
		return out
	end

	local formatStatement = function(statement)
		local out = ""
		if statement.AstType == 'VerbatimCode' then
			out = getIndentation(statement)..statement.Data
		elseif statement.AstType == 'AssignmentStatement' then
			out = getIndentation(statement)
			for i = 1, #statement.Lhs do
				out = out..formatExpr(statement.Lhs[i])
				if i ~= #statement.Lhs then
					out = out..", "
				end
			end
			if #statement.Rhs > 0 then
				out = out.." = "
				for i = 1, #statement.Rhs do
					out = out..formatExpr(statement.Rhs[i])
					if i ~= #statement.Rhs then
						out = out..", "
					end
				end
			end
		elseif statement.AstType == 'CallStatement' then
			out = getIndentation(statement) .. formatExpr(statement.Expression)
		elseif statement.AstType == 'LocalStatement' then
			out = getIndentation(statement) .. out.."local "
			for i = 1, #statement.LocalList do
				out = out..statement.LocalList[i]
				if i ~= #statement.LocalList then
					out = out..", "
				end
			end
			if #statement.InitList > 0 then
				out = out.." = "
				for i = 1, #statement.InitList do
					out = out..formatExpr(statement.InitList[i])
					if i ~= #statement.InitList then
						out = out..", "
					end
				end
			end
		elseif statement.AstType == 'IfStatement' then
			out = getIndentation(statement) .. joinStatementsSafe("if ", formatExpr(statement.Clauses[1].Condition))
			out = joinStatementsSafe(out, " then") .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Clauses[1].Body))
			indent = indent - 1
			for i = 2, #statement.Clauses do
				local st = statement.Clauses[i]
				if st.Condition then
					out = getIndentation(statement) .. joinStatementsSafe(out, getIndentation(statement) .. "elseif ")
					out = joinStatementsSafe(out, formatExpr(st.Condition))
					out = joinStatementsSafe(out, " then") .. " "
				else
					out = joinStatementsSafe(out, getIndentation(statement) .. "else") .. " "
				end
				indent = indent + 1
				out = joinStatementsSafe(out, formatStatlist(st.Body))
				indent = indent - 1
			end
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'WhileStatement' then
			out = getIndentation(statement) .. joinStatementsSafe("while ", formatExpr(statement.Condition))
			out = joinStatementsSafe(out, " do") .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'DoStatement' then
			out = getIndentation(statement) .. joinStatementsSafe(out, "do") .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'ReturnStatement' then
			out = getIndentation(statement) .. "return "
			for i = 1, #statement.Arguments do
				out = joinStatementsSafe(out, formatExpr(statement.Arguments[i]))
				if i ~= #statement.Arguments then
					out = out..", "
				end
			end
		elseif statement.AstType == 'BreakStatement' then
			out = getIndentation(statement) .. "break"
		elseif statement.AstType == 'RepeatStatement' then
			out = getIndentation(statement) .. "repeat" .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "until ")
			out = joinStatementsSafe(out, formatExpr(statement.Condition)) .. " "
		elseif statement.AstType == 'Function' then
			if statement.IsLocal then
				out = "local "
			end
			out = joinStatementsSafe(out, "function ")
			out = getIndentation(statement) .. out
			if statement.IsLocal then
				out = out..statement.Name
			else
				out = out..formatExpr(statement.Name)
			end
			out = out.."("
			if #statement.Arguments > 0 then
				for i = 1, #statement.Arguments do
					out = out..statement.Arguments[i]
					if i ~= #statement.Arguments then
						out = out..", "
					elseif statement.VarArg then
						out = out..",..."
					end
				end
			elseif statement.VarArg then
				out = out.."..."
			end
			out = out..")" .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'GenericForStatement' then
			out = getIndentation(statement) .. "for "
			for i = 1, #statement.VariableList do
				out = out..statement.VariableList[i]
				if i ~= #statement.VariableList then
					out = out..", "
				end
			end
			out = out.." in "
			for i = 1, #statement.Generators do
				out = joinStatementsSafe(out, formatExpr(statement.Generators[i]))
				if i ~= #statement.Generators then
					out = joinStatementsSafe(out, ', ')
				end
			end
			out = joinStatementsSafe(out, " do") .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'NumericForStatement' then
			out = getIndentation(statement) .. "for "
			out = out..statement.Variable.." = "
			out = out..formatExpr(statement.Start)..", "..formatExpr(statement.End)
			if statement.Step then
				out = out..", "..formatExpr(statement.Step)
			end
			out = joinStatementsSafe(out, " do") .. " "
			indent = indent + 1
			out = joinStatementsSafe(out, formatStatlist(statement.Body))
			indent = indent - 1
			out = joinStatementsSafe(out, getIndentation(statement) .. "end") .. " "
		elseif statement.AstType == 'LabelStatement' then
			out = getIndentation(statement) .. "::" .. statement.Label .. "::" .. " "
		elseif statement.AstType == 'GotoStatement' then
			out = getIndentation(statement) .. "goto " .. statement.Label .. " "
		elseif statement.AstType == 'Comment' then
			if statement.CommentType == 'Shebang' then
				out = getIndentation(statement) .. statement.Data
				--out = out .. EOL
			elseif statement.CommentType == 'Comment' then
				out = getIndentation(statement) .. statement.Data
				--out = out .. EOL
			elseif statement.CommentType == 'LongComment' then
				out = getIndentation(statement) .. statement.Data
				--out = out .. EOL
			end
		elseif statement.AstType == 'Eof' then
			-- Ignore
		else
			print("Unknown AST Type: ", statement.AstType)
		end
		return out
	end

	formatStatlist = function(statList)
		local out = ''
		for _, stat in pairs(statList.Body) do
			out = joinStatementsSafe(out, formatStatement(stat) .. " ")
		end
		return out
	end

	return formatStatlist(ast)
end

return Format_Beautify
