local utils = require("utils")

local astTransform = {}

function astTransform.pullFuncExpr(expr)
    local yield = coroutine.yield

    if expr.__tag == "FunctionExpr" then
        expr.body = astTransform.pullFuncStmtList(expr.body)
        return expr
    elseif expr.__tag == "ParenExpr" then
        expr.inner = astTransform.pullFuncExpr(expr.inner)
        return expr
    elseif expr.__tag == "VarExpr" then
        return expr
    elseif expr.__tag == "MemberExpr" then
        expr.base = astTransform.pullFuncExpr(expr.base)
        return expr
    elseif expr.__tag == "IndexExpr" then
        expr.base = astTransform.pullFuncExpr(expr.base)        
        expr.index = astTransform.pullFuncExpr(expr.index)
        return expr
    elseif expr.__tag == "CallExpr" then
        expr.base = astTransform.pullFuncExpr(expr.base)
        for i, arg in ipairs(expr.args) do
            expr.args[i] = astTransform.pullFuncExpr(arg)
        end
        return yield(expr)
    elseif expr.__tag == "StringCallExpr" then
        expr.base = astTransform.pullFuncExpr(expr.base)
        return yield(expr)
    elseif expr.__tag == "TableCallExpr" then
        expr.base = astTransform.pullFuncExpr(expr.base)
        expr.arg = astTransform.pullFuncExpr(expr.arg)        
        return yield(expr)
    elseif expr.__tag == "NumberExpr" then
        return expr
    elseif expr.__tag == "StringExpr" then
        return expr
    elseif expr.__tag == "NilExpr" then
        return expr
    elseif expr.__tag == "BooleanExpr" then
        return expr
    elseif expr.__tag == "DotsExpr" then
        return expr
    elseif expr.__tag == "ConstructorExpr" then
        for i, elem in ipairs(expr.entry_list) do
            if elem.__tag == "Key" then
                expr.entry_list[i].key = astTransform.pullFuncExpr(elem.key)
                expr.entry_list[i].value = astTransform.pullFuncExpr(elem.value)
            elseif elem.__tag == "KeyString" or elem.__tag == "Value" then
                expr.entry_list[i].value = astTransform.pullFuncExpr(elem.value)
            end
        end
        return expr
    elseif expr.__tag == "UnopExpr" then
        expr.rhs = astTransform.pullFuncExpr(expr.rhs)
        return expr
    elseif expr.__tag == "BinopExpr" then
        expr.lhs = astTransform.pullFuncExpr(expr.lhs)
        expr.rhs = astTransform.pullFuncExpr(expr.rhs)
        return expr
    end
end

local function gatherLocals(expr, locals)
    local freshVar = utils.nameGenerator("t")
    local exprGen = coroutine.create(function()
        return astTransform.pullFuncExpr(expr)
    end)
    local ok, newExpr = coroutine.resume(exprGen)
    while coroutine.status(exprGen) ~= "dead" do
        local tmpVar = {
            __tag = "VarExpr",
            name = freshVar()
        }
        if ok and newExpr then
            -- TODO: wrap this in table.pack and table.unpack at use site
            locals[#locals+1] = {
                __tag = "LocalStatement",
                init_exprs = {newExpr},
                names = {tmpVar.name},
                fcall = true,
            }
        elseif not ok then
            error(expr)
        end
        ok, newExpr = coroutine.resume(exprGen, tmpVar)
    end
    if ok then
        return newExpr
    else
        error(newExpr)
    end
end

function astTransform.pullFuncStmt(stmt)
    local locals = {}

    if stmt.__tag == "IfStatement" then
        for i, clause in ipairs(stmt.clauses) do
            clause.cond = gatherLocals(clause.cond, locals)
            clause.body = astTransform.pullFuncStmtList(clause.body)
        end
        -- if clause.else_body.__tag == "Some" then
        --     clause.else_body
        -- end
    elseif stmt.__tag == "WhileStatement" then
        stmt.cond = gatherLocals(stmt.cond, locals)
        stmt.body = astTransform.pullFuncStmtList(stmt.body)
    elseif stmt.__tag == "DoStatement" then
        stmt.body = astTransform.pullFuncStmtList(stmt.body)
    elseif stmt.__tag == "NumericForStatement" then
        stmt.start = gatherLocals(stmt.start, locals)
        stmt.finish = gatherLocals(stmt.finish, locals)
        -- TODO: step
        stmt.body = astTransform.pullFuncStmtList(stmt.body) 
    elseif stmt.__tag == "GenericForStatement" then
        for i, expr in ipairs(stmt.generators) do
            stmt.generators[i] = gatherLocals(expr, locals)
        end
        stmt.body = astTransform.pullFuncStmtList(stmt.body) 
    elseif stmt.__tag == "RepeatStatement" then
        stmt.cond = gatherLocals(stmt.cond, locals)
        stmt.body = astTransform.pullFuncStmtList(stmt.body) 
    elseif stmt.__tag == "FunctionStatement" then
        stmt.name = gatherLocals(stmt.name, locals)
        stmt.f.body = astTransform.pullFuncStmtList(stmt.f.body)
    elseif stmt.__tag == "LocalStatement" then
        for i, expr in ipairs(stmt.init_exprs) do
            stmt.init_exprs[i] = gatherLocals(expr, locals)
        end
    elseif stmt.__tag == "LabelStatement" then
        
    elseif stmt.__tag == "ReturnStatement" then
        for i, expr in ipairs(stmt.args) do
            stmt.args[i] = gatherLocals(expr, locals)
        end
    elseif stmt.__tag == "BreakStatement" then

    elseif stmt.__tag == "GotoStatement" then

    elseif stmt.__tag == "AssignmentStatement" then
        for i, expr in ipairs(stmt.lhs) do
            stmt.lhs[i] = gatherLocals(expr, locals)
        end
        for i, expr in ipairs(stmt.rhs) do
            stmt.rhs[i] = gatherLocals(expr, locals)
        end
    elseif stmt.__tag == "CallStatement" then
        stmt.base = gatherLocals(stmt.base, locals)
        for i, arg in ipairs(stmt.args) do
            stmt.args[i] = gatherLocals(arg, locals)
        end
    elseif stmt.__tag == "Eof" then
        
    end

    return locals
end

function astTransform.pullFuncStmtList(stmtList)
    local stmts = {}
    for i, stmt in ipairs(stmtList) do
        for j, substmt in ipairs(astTransform.pullFuncStmt(stmt)) do
            stmts[#stmts+1] = substmt
        end
        stmts[#stmts+1] = stmt
    end
    return stmts
end

local freshLabel = utils.nameGenerator("L")
function astTransform.insertGotos(stmtList)
    local stmts = {}
    for i, stmt in ipairs(stmtList) do
        stmts[#stmts+1] = stmt
        if stmt.__tag == "IfStatement" then
            for _, clause in ipairs(stmt.clauses) do
                utils.append(stmts, astTransform.insertGotos(clause.body))
            end
        elseif stmt.__tag == "WhileStatement" then
            utils.append(stmts, astTransform.insertGotos(stmt.body))
        elseif stmt.__tag == "NumericForStatement" then
            utils.append(stmts, astTransform.insertGotos(stmt.body))
        elseif stmt.__tag == "GenericForStatement" then
            utils.append(stmts, astTransform.insertGotos(stmt.body))
        elseif stmt.__tag == "RepeatStatement" then
            utils.append(stmts, astTransform.insertGotos(stmt.body))
        elseif stmt.__tag == "FunctionStatement" then
            utils.append(stmts, astTransform.insertGotos(stmt.f.body))
        elseif stmt.__tag == "LocalStatement" then
            if i ~= #stmtList then
                local label = freshLabel()
                stmts[#stmts+1] = {
                    __tag = "GotoStatement",
                    label = label
                }
                stmts[#stmts+1] = {
                    __tag = "LabelStatement",
                    label = label
                }
            end
        elseif stmt.__tag == "LabelStatement" then
            
        elseif stmt.__tag == "ReturnStatement" then

        elseif stmt.__tag == "BreakStatement" then
    
        elseif stmt.__tag == "GotoStatement" then
    
        elseif stmt.__tag == "AssignmentStatement" then

        elseif stmt.__tag == "CallStatement" then

        elseif stmt.__tag == "Eof" then
            
        end

        -- TODO: insert goto after every call not in tail position
        -- make sure to handle loops correctly
    end
    return stmts
end

return astTransform