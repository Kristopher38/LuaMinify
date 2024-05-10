import haxe.ds.Option;
import lua.Table;

using Lambda;

enum FunctionDef {
    FuncDef (args: Array<String>, vararg: Bool, body: Array<Stmt>);
}

enum TableElem {
    Key (key: Expr, value: Expr);
    KeyString (key: String, value: Expr);
    Value (value: Expr);
}

enum Expr {
    FunctionExpr (f: FunctionDef);
    VarExpr (name: String);
    MemberExpr (base: Expr, indexer: String, ident: String);
    IndexExpr (base: Expr, index: Expr);
    CallExpr (base: Expr, args: Array<Expr>);
    NumberExpr (value: Int);
    StringExpr (value: String);
    NilExpr;
    BooleanExpr (value: Bool);
    DotsExpr;
    ConstructorExpr (entryList: Array<TableElem>);
    UnopExpr (rhs: Expr, op: String);
    BinopExpr (lhs: Expr, op: String, rhs: Expr);
}

enum Stmt {
    IfStatement (cond: Expr, thenBody: Array<Stmt>, elseBody: Array<Stmt>);
    WhileStatement (cond: Expr, body: Array<Stmt>);
    DoStatement (body: Array<Stmt>);
    NumericForStatement (variable: String, start: Expr, finish: Expr, step: Option<Expr>, body: Array<Stmt>);
    GenericForStatement (varList: Array<String>, generators: Array<Expr>, body: Array<Stmt>);
    RepeatStatement (cond: Expr, body: Array<Stmt>);
    FunctionStatement (name: Expr, isLocal: Bool, f: FunctionDef);
    LocalStatement (names: Array<String>, initExprs: Array<Expr>);
    LabelStatement (label: String);
    ReturnStatement (args: Array<Expr>);
    BreakStatement;
    GotoStatement (label: String);
    AssignmentStatement (lhs: Array<Expr>, rhs: Array<Expr>);
    CallStatement (base: Expr, args: Array<Expr>);
}


@:luaRequire("ParseLua") 
extern class Parser {
    static function Init<T>(
        array: Class<Array<T>>,
        option: Enum<Option<T>>,
        stmt: Enum<Stmt>,
        expr: Enum<Expr>,
        tableElem: Enum<TableElem>,
        functionDef: Enum<FunctionDef>
    ): Void;
    static function ParseLua(str: String, options: lua.Table<String, Dynamic>): Array<Stmt>;
}


class NameGenerator {
    var state: Int;
    var prefix: String;

    public function new(prefix: String) {
        this.state = 0;
        this.prefix = prefix;
    }

    public function next(): String {
        var name = '$prefix$state';
        state++;
        return name;
    }
}


class PullFunction {
    var freshVar: NameGenerator;

    public function new(freshVar: NameGenerator) {
        this.freshVar = freshVar;
    }

    function tablePack(expr: Expr): Expr {
        return CallExpr(MemberExpr(StringExpr("table"), ".", "pack"), [expr]);
    }

    function tableUnpack(expr: Expr): Expr {
        return CallExpr(MemberExpr(StringExpr("table"), ".", "unpack"), [expr]);
    }

    function pullFunc(f: FunctionDef, pulled: OrderedMap<String, Expr>): FunctionDef {
        return switch (f) {
            case FuncDef(args, vararg, body):
                FuncDef(args, vararg, pullStmts(body));
        }
    }

    function pullTableElem(elem: TableElem, pulled: OrderedMap<String, Expr>): TableElem {
        var pullExpr = pullExpr.bind(_, pulled);

        return switch (elem) {
            case Key(key, value):
                Key(pullExpr(key), pullExpr(value));
            case KeyString(key, value):
                KeyString(key, pullExpr(value));
            case Value(value):
                Value(pullExpr(value));
        }
    }

    public function pullStmts(stmts: Array<Stmt>): Array<Stmt> {
        var newStmts = new Array();
        for (stmt in stmts) {
            var pulled = new OrderedMap();
            var newStmt = pullStmt(stmt, pulled);
            for (name => expr in pulled)
                newStmts.push(LocalStatement([name], [expr]));
                // newStmts.push(LocalStatement([name], [tablePack(expr)]));
            newStmts.push(newStmt);
        }
        return newStmts;
    }

    function pullStmt(stmt: Stmt, pulled: OrderedMap<String, Expr>): Stmt {
        var pullExpr = pullExpr.bind(_, pulled);
        var pullFunc = pullFunc.bind(_, pulled);

        return switch (stmt) {
            case IfStatement(cond, thenBody, elseBody):
                IfStatement(pullExpr(cond), pullStmts(thenBody), pullStmts(elseBody));
            case WhileStatement(cond, body):
                WhileStatement(pullExpr(cond), pullStmts(body));
            case DoStatement(body):
                DoStatement(pullStmts(body));
            case NumericForStatement(variable, start, finish, Some(step), body):
                NumericForStatement(variable, pullExpr(start), pullExpr(finish), Some(pullExpr(step)), pullStmts(body));
            case NumericForStatement(variable, start, finish, None, body):
                NumericForStatement(variable, pullExpr(start), pullExpr(finish), None, pullStmts(body));
            case GenericForStatement(varList, generators, body):
                GenericForStatement(varList, generators.map(pullExpr), pullStmts(body));
            case RepeatStatement(cond, body):
                RepeatStatement(pullExpr(cond), pullStmts(body));
            case FunctionStatement(name, isLocal, f):
                FunctionStatement(pullExpr(name), isLocal, pullFunc(f));
            case LocalStatement(names, initExprs):
                LocalStatement(names, initExprs.map(pullExpr));
            case ReturnStatement(args):
                ReturnStatement(args.map(pullExpr));
            case AssignmentStatement(lhs, rhs):
                AssignmentStatement(lhs.map(pullExpr), rhs.map(pullExpr));
            case CallStatement(base, args):
                CallStatement(pullExpr(base), args.map(pullExpr));
            case LabelStatement(_) | BreakStatement | GotoStatement(_):
                stmt;
        }
    }

    function pullExpr(expr: Expr, pulled: OrderedMap<String, Expr>): Expr {
        var pullExpr = pullExpr.bind(_, pulled);
        var pullFunc = pullFunc.bind(_, pulled);
        var pullTableElem = pullTableElem.bind(_, pulled);

        var converted = switch (expr) {
            case FunctionExpr(f):
                FunctionExpr(pullFunc(f));
            case MemberExpr(base, indexer, ident):
                MemberExpr(pullExpr(base), indexer, ident);
            case IndexExpr(base, index):
                IndexExpr(pullExpr(base), pullExpr(index));
            case CallExpr(base, args):
                CallExpr(pullExpr(base), args.map(pullExpr));
            case ConstructorExpr(entryList):
                ConstructorExpr(entryList.map(pullTableElem));
            case UnopExpr(rhs, op):
                UnopExpr(pullExpr(rhs), op);
            case BinopExpr(lhs, op, rhs):
                BinopExpr(pullExpr(lhs), op, pullExpr(rhs));
            case NumberExpr(_) | StringExpr(_) | NilExpr | BooleanExpr(_) | DotsExpr | VarExpr(_):
                expr;
        }
        switch (expr) {
            case CallExpr(_, _):
                var name = freshVar.next();
                pulled[name] = converted;
                return VarExpr(name);
                // return tableUnpack(VarExpr(name));
            case _: return converted;
        }
    }
}

class InsertGotos {
    var freshVar: NameGenerator;
    var freshLabel: NameGenerator;

    public function new(freshVar: NameGenerator, freshLabel: NameGenerator) {
        this.freshVar = freshVar;
        this.freshLabel = freshLabel;
    }

    function insertFunc(f: FunctionDef, breakLabel: Option<String>): FunctionDef {
        return switch (f) {
            case FuncDef(args, vararg, body):
                FuncDef(args, vararg, body.flatMap(insertStmt.bind(_, breakLabel)));
        }
    }

    function hasLastReturn(stmts: Array<Stmt>): Bool {
        if (stmts.length == 0)
            return false;
        return switch (stmts[stmts.length - 1]) {
            case ReturnStatement(_): true;
            case _: false;
        }
    }

    function insertExpr(expr: Expr, breakLabel: Option<String>): Expr {
        return switch (expr) {
            case FunctionExpr(f):
                FunctionExpr(insertFunc(f, breakLabel));
            case _: expr;
        }
    }

    function insertStmt(stmt: Stmt, breakLabel: Option<String>): Array<Stmt> {
        var _insertStmt = insertStmt.bind(_, breakLabel);
        var _insertFunc = insertFunc.bind(_, breakLabel);
        var _insertExpr = insertExpr.bind(_, breakLabel);

        return switch (stmt) {
            case IfStatement(cond, thenBody, elseBody):
                var endLabel = freshLabel.next();
                if (!hasLastReturn(thenBody))
                    thenBody.push(GotoStatement(endLabel));
                if (!hasLastReturn(elseBody) && elseBody.length > 0)
                    elseBody.push(GotoStatement(endLabel));
                [
                    IfStatement(_insertExpr(cond), thenBody.flatMap(_insertStmt), elseBody.flatMap(_insertStmt)),
                    LabelStatement(endLabel),
                ];
            case WhileStatement(cond, body):
                var whileLabel = freshLabel.next();
                var breakLabel = freshLabel.next();
                if (!hasLastReturn(body))
                    body.push(GotoStatement(whileLabel));
                [
                    LabelStatement(whileLabel),
                    IfStatement(_insertExpr(cond), body.flatMap(insertStmt.bind(_, Some(breakLabel))), []),
                    LabelStatement(breakLabel)
                ];
            case DoStatement(body):
                [DoStatement(body.flatMap(_insertStmt))];
            case NumericForStatement(variable, start, finish, step, body):
                var forLabel = freshLabel.next();
                var breakLabel = freshLabel.next();
                var finishVar = freshVar.next();
                var stepVar = freshVar.next();
                var stepExpr = switch (step) {
                    case Some(step): step;
                    case None: NumberExpr(1);
                };
                body.push(AssignmentStatement([VarExpr(variable)], [BinopExpr(VarExpr(variable), "+", stepExpr)]));
                if (!hasLastReturn(body))
                    body.push(GotoStatement(forLabel));
                [
                    LocalStatement([variable, finishVar, stepVar], [start, finish, stepExpr]),
                    LabelStatement(forLabel),
                    IfStatement(BinopExpr(VarExpr(variable), "<=", VarExpr(finishVar)), body.flatMap(insertStmt.bind(_, Some(breakLabel))), []),
                    LabelStatement(breakLabel),
                ];
            case GenericForStatement(varList, generators, body):
                var forLabel = freshLabel.next();
                var breakLabel = freshLabel.next();
                var iterFunc = freshVar.next();
                var invState = freshVar.next();
                var ctrlVar = freshVar.next();
                if (!hasLastReturn(body))
                    body.push(GotoStatement(forLabel));
                [
                    LocalStatement([iterFunc, invState, ctrlVar], generators.map(_insertExpr)),
                    LabelStatement(forLabel),
                    LocalStatement(varList, [CallExpr(VarExpr(iterFunc), [VarExpr(invState), VarExpr(ctrlVar)])]),
                    AssignmentStatement([VarExpr(ctrlVar)], [VarExpr(varList[0])]),
                    IfStatement(BinopExpr(VarExpr(ctrlVar),  "==", NilExpr), body.flatMap(insertStmt.bind(_, Some(breakLabel))), []),
                    LabelStatement(breakLabel),
                ];
            case RepeatStatement(cond, body):
                var repeatLabel = freshLabel.next();
                var breakLabel = freshLabel.next();
                if (!hasLastReturn(body))
                    body.push(GotoStatement(repeatLabel));
                [
                    LabelStatement(repeatLabel),
                    DoStatement(body.flatMap(insertStmt.bind(_, Some(breakLabel)))),
                    IfStatement(_insertExpr(cond), [GotoStatement(repeatLabel)], []),
                    LabelStatement(breakLabel),
                ];
            case FunctionStatement(name, isLocal, f):
                [FunctionStatement(name, isLocal, insertFunc(f, breakLabel))];
            case CallStatement(base, args):
                // TODO: only insert gotos after functions that are known during transformation
                var label = freshLabel.next();
                [
                    CallStatement(base, args.map(_insertExpr)),
                    GotoStatement(label),
                    LabelStatement(label),
                ];
            case BreakStatement:
                switch (breakLabel) {
                    case Some(label): [GotoStatement(label)];
                    case None: [];
                }
            case LocalStatement([name], [CallExpr(base, args)]):
                // TODO: only insert gotos after functions that are known during transformation
                var label = freshLabel.next();
                [
                    LocalStatement([name], [CallExpr(base, args.map(_insertExpr))]),
                    GotoStatement(label),
                    LabelStatement(label),
                ];
            case LocalStatement(names, initExprs):
                [LocalStatement(names, initExprs.map(_insertExpr))];
            case ReturnStatement(args):
                [ReturnStatement(args.map(_insertExpr))];
            case AssignmentStatement(lhs, rhs):
                [AssignmentStatement(lhs.map(_insertExpr), rhs.map(_insertExpr))];
            case LabelStatement(_) | GotoStatement(_):
                [stmt];
        }
    }

    public function insertStmts(stmts: Array<Stmt>): Array<Stmt> {
        return stmts.flatMap(insertStmt.bind(_, None));
    }
}

class LuaParse {
    static function tab(indent: Int): String {
        return StringTools.rpad("", " ", indent * 4);
    }

    static function te2str(tableElem: TableElem, indent: Int): String {
        var e2str = e2str.bind(_, indent);

        return switch (tableElem) {
            case Key(key, value): '[${e2str(key)}] = ${e2str(value)}';
            case KeyString(key, value): '$key = ${e2str(value)}';
            case Value(value): '${e2str(value)}';
        }
    }

    static function e2str(expr: Expr, indent: Int): String {
        var e2str = e2str.bind(_, indent);
        var f2str = f2str.bind(_, indent);
        var te2str = te2str.bind(_, indent);

        return switch (expr) {
            case FunctionExpr(f): 'function ${f2str(f)}\n${tab(indent)}end';
            case VarExpr(name): '$name';
            case MemberExpr(base, indexer, ident): '${e2str(base)}$indexer$ident';
            case IndexExpr(base, index): '${e2str(base)}[${e2str(index)}]';
            case CallExpr(base, args): '${e2str(base)}(${args.map(e2str).join(", ")})';
            case NumberExpr(value): '$value';
            case StringExpr(value): '"$value"';
            case NilExpr: 'nil';
            case BooleanExpr(value): value ? 'true' : 'false';
            case DotsExpr: '...';
            case ConstructorExpr(entryList):
                if (entryList.length >= 5)
                    '{\n${tab(indent + 1)}${entryList.map(te2str).join(',\n${tab(indent + 1)}')}\n${tab(indent)}}';
                else
                    '{${entryList.map(te2str).join(", ")}}';
            case UnopExpr(rhs, op): '($op ${e2str(rhs)})';
            case BinopExpr(lhs, op, rhs): '(${e2str(lhs)} $op ${e2str(rhs)})'; // TODO: implement least-parens printing
        }
    }

    static function sta2str(stmtArray: Array<Stmt>, indent: Int = -1): String {
        var st2str = st2str.bind(_, indent + 1);
        return stmtArray.map(st2str).join("\n");
    }

    static function f2str(f: FunctionDef, indent: Int): String {
        var sta2str = sta2str.bind(_, indent);
        return switch (f) {
            case FuncDef(args, vararg, body):
                if (vararg)
                    args.push("...");
                '(${args.join(", ")})\n${sta2str(body)}';
        }
    }

    static function st2str(stmt: Stmt, indent: Int): String {
        var e2str = e2str.bind(_, indent);
        var f2str = f2str.bind(_, indent);
        var st2str = st2str.bind(_, indent);
        var sta2str = sta2str.bind(_, indent);

        return tab(indent) + switch (stmt) {
            case IfStatement(cond, thenBody, []):
                'if ${e2str(cond)} then\n${sta2str(thenBody)}\n${tab(indent)}end';
            case IfStatement(cond, thenBody, [nextIf = IfStatement(_, _, _)]):
                'if ${e2str(cond)} then\n${sta2str(thenBody)}\n${tab(indent)}else${StringTools.ltrim(st2str(nextIf))}'; // force no indent
            case IfStatement(cond, thenBody, elseBody):
                'if ${e2str(cond)} then\n${sta2str(thenBody)}\n${tab(indent)}else\n${sta2str(elseBody)}\n${tab(indent)}end';
            case WhileStatement(cond, body):
                'while ${e2str(cond)} do\n${sta2str(body)}\n${tab(indent)}end';
            case DoStatement(body):
                'do\n${sta2str(body)}\n${tab(indent)}end';
            case NumericForStatement(variable, start, finish, Some(step), body):
                'for $variable = ${e2str(start)}, ${e2str(finish)}, ${e2str(step)} do\n${sta2str(body)}\n${tab(indent)}end';
            case NumericForStatement(variable, start, finish, None, body):
                'for $variable = ${e2str(start)}, ${e2str(finish)} do\n${sta2str(body)}\n${tab(indent)}end';
            case GenericForStatement(varList, generators, body):
                'for ${varList.join(", ")} in ${generators.map(e2str).join(", ")} do\n${sta2str(body)}\n${tab(indent)}end';
            case RepeatStatement(cond, body):
                'repeat\n${sta2str(body)}\n${tab(indent)}until ${e2str(cond)}';
            case FunctionStatement(name, isLocal, f):
                '${isLocal ? 'local ' : ''}function ${e2str(name)}${f2str(f)}\n${tab(indent)}end';
            case LocalStatement(names, []):
                'local ${names.join(", ")}';
            case LocalStatement(names, initExprs):
                'local ${names.join(", ")} = ${initExprs.map(e2str).join(", ")}';
            case LabelStatement(label):
                '::$label::';
            case ReturnStatement(args):
                'return ${args.map(e2str).join(", ")}';
            case BreakStatement:
                'break';
            case GotoStatement(label):
                'goto $label';
            case AssignmentStatement(lhs, rhs):
                '${lhs.map(e2str).join(", ")} = ${rhs.map(e2str).join(", ")}';
            case CallStatement(base, args):
                '${e2str(base)}(${args.map(e2str).join(", ")})';
        }
    }

    static function main() {
        Parser.Init(Array, Option, Stmt, Expr, TableElem, FunctionDef);
        var options = lua.Table.fromMap([
            "disableEmitLeadingWhite" => true,
            "disableEmitTokenList" => true
        ]);
        var data = sys.io.File.getContent("ParseLua.lua");
        var parsed = Parser.ParseLua(data, options);
        var varGenerator = new NameGenerator("t");
        var labelGenerator = new NameGenerator("l");
        var pullTransformer = new PullFunction(varGenerator);
        var transformed = pullTransformer.pullStmts(parsed);
        var gotoTransformer = new InsertGotos(varGenerator, labelGenerator);
        var gotosInserted = gotoTransformer.insertStmts(transformed);
        Sys.print(sta2str(gotosInserted));
    }
}