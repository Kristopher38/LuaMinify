open import "prelude.ml"
open import "./rawarray.ml"

type table_elem 'a 'b =
| Key of {key: 'a, value: 'b}
| KeyString of {key: string, value: 'b}
| Value of {value: 'b}

type func_def 'stmt <- {args: rawarray string, vararg: bool, body: rawarray 'stmt}
type func_call 'expr 's <- {base: 'expr 's, args: rawarray ('expr 's)}

type expr 's =
| FunctionExpr of func_def 's
| ParenExpr of {inner: expr 's}
| VarExpr of {name: string}
| MemberExpr of {base: expr 's, indexer: string, ident: string}
| IndexExpr of {base: expr 's, index: expr 's}
| CallExpr of func_call expr 's
| StringCallExpr of {base: expr 's, arg: string}
| TableCallExpr of {base: expr 's, arg: expr 's} (* arg should always be ConstructorExpr *) 
| NumberExpr of {value: int}
| StringExpr of {value: string}
| NilExpr
| BooleanExpr of {value: bool}
| DotsExpr
| ConstructorExpr of {entry_list: rawarray (table_elem (expr 's) (expr 's))}
| UnopExpr of {rhs: expr 's, op: string, op_prec: int}
| BinopExpr of {lhs: expr 's, op: string, op_prec: int, rhs: expr 's}

type stmt =
| IfStatement of {clauses: rawarray {cond: expr stmt, body: rawarray stmt}, else_body: option (rawarray stmt)}
| WhileStatement of {cond: expr stmt, body: rawarray stmt}
| DoStatement of {body: rawarray stmt}
| NumericForStatement of {var: string, start: expr stmt, finish: expr stmt, step: option (expr stmt), body: rawarray stmt}
| GenericForStatement of {var_list: rawarray string, generators: rawarray (expr stmt), body: rawarray stmt}
| RepeatStatement of {cond: expr stmt, body: rawarray stmt}
| FunctionStatement of {name: string, is_local: bool, f: func_def stmt}
| LocalStatement of {names: rawarray string, init_exprs: rawarray (expr stmt)}
| LabelStatement of {label: string}
| ReturnStatement of {args: rawarray (expr stmt)}
| BreakStatement
| GotoStatement of {label: string}
| AssignmentStatement of {lhs: expr stmt, rhs: expr stmt}
| CallStatement of func_call expr stmt
| Eof

type stmt_list <- rawarray stmt


external val mock : unit -> expr stmt = "function() return require(\"mock\") end"

external val inspect : _ -> unit = "function(x) print(require(\"inspect\")(x)) end"


let _ = match mock () with
| UnopExpr {rhs = CallExpr {base = MemberExpr {base, ident, indexer}, args}, op, op_prec} ->
  iter (fun arg -> match arg with
  | StringExpr {value} -> put_line value
  | VarExpr {name} -> put_line name
  | _ -> put_line "other_expr") args
| _ -> put_line "other"