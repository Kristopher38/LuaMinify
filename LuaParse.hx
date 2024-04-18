import haxe.ds.Option;
import lua.Table;

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
    StringExpr (value: String); // TODO: strip "" from value in ParseLua
    NilExpr;
    BooleanExpr (value: Bool);
    DotsExpr;
    ConstructorExpr (entryList: Array<TableElem>);
    UnopExpr (rhs: Expr, op: String, opPrec: Int);
    BinopExpr (lhs: Expr, op: String, opPrec: Int, rhs: Expr);
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


class LuaParse {
    static function te2str(tableElem: TableElem): String {
        return switch (tableElem) {
            case Key(key, value): '[${e2str(key)}] = ${e2str(value)}';
            case KeyString(key, value): '$key = ${e2str(value)}';
            case Value(value): '${e2str(value)}';
        }
    }

    static function e2str(expr: Expr): String {
        return switch (expr) {
            case FunctionExpr(f): 'function ${f2str(f)} end';
            case VarExpr(name): '$name';
            case MemberExpr(base, indexer, ident): '${e2str(base)}$indexer$ident';
            case IndexExpr(base, index): '${e2str(base)}[${e2str(index)}]';
            case CallExpr(base, args): '${e2str(base)}(${args.map(e2str).join(", ")})';
            case NumberExpr(value): '$value';
            case StringExpr(value): '$value';
            case NilExpr: 'nil';
            case BooleanExpr(value): value ? 'true' : 'false';
            case DotsExpr: '...';
            case ConstructorExpr(entryList): '{${entryList.map(te2str).join(", ")}}';
            case UnopExpr(rhs, op, opPrec): '$op ${e2str(rhs)}';
            case BinopExpr(lhs, op, opPrec, rhs): '${e2str(lhs)} $op ${e2str(rhs)}';
        }
    }

    static function sta2str(stmtArray: Array<Stmt>): String {
        return stmtArray.map(st2str).join("\n");
    }

    static function f2str(f: FunctionDef): String {
        return switch (f) {
            case FuncDef(args, vararg, body):
                if (vararg)
                    args.push("...");
                '(${args.join(", ")}) ${sta2str(body)}';
        }
    }

    static function st2str(stmt: Stmt): String {
        return switch (stmt) {
            case IfStatement(cond, thenBody, []):
                'if ${e2str(cond)} then ${sta2str(thenBody)} end';
            case IfStatement(cond, thenBody, [nextIf = IfStatement(_, _, _)]):
                'if ${e2str(cond)} then ${sta2str(thenBody)} else${st2str(nextIf)}';
            case IfStatement(cond, thenBody, elseBody):
                'if ${e2str(cond)} then ${sta2str(thenBody)} else ${sta2str(elseBody)} end';
            case WhileStatement(cond, body):
                'while ${e2str(cond)} do ${sta2str(body)} end';
            case DoStatement(body):
                'do ${sta2str(body)} end';
            case NumericForStatement(variable, start, finish, Some(step), body):
                'for $variable = ${e2str(start)}, ${e2str(finish)}, ${e2str(step)} do ${sta2str(body)} end';
            case NumericForStatement(variable, start, finish, None, body):
                'for $variable = ${e2str(start)}, ${e2str(finish)} do ${sta2str(body)} end';
            case GenericForStatement(varList, generators, body):
                'for ${varList.join(", ")} in ${generators.map(e2str).join(", ")} do ${sta2str(body)} end';
            case RepeatStatement(cond, body):
                'repeat ${sta2str(body)} until ${e2str(cond)}';
            case FunctionStatement(name, isLocal, f):
                '${isLocal ? 'local ' : ''}function ${e2str(name)}${f2str(f)} end';
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
        Sys.print(sta2str(parsed));
    }
}