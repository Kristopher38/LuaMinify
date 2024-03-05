open import "prelude.ml"
open import "amulet/exception.ml"
open import "amulet/base/lua.ml"
open import "lua/io.ml"
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


external val parse_lua : string -> stmt_list = "function(s) local ok, ast = require(\"ParseLua\")(s) return ast end"

external val inspect : _ -> unit = "function(x) print(require(\"inspect\")(x)) end"

let parse_file filename =
  let f = open_file filename Read_m in
  let lines = read_all f in
  match lines with
  | None -> throw (IoError "fail")
  | Some str -> parse_lua str

let delim f del arr =
  foldr (fun elem acc -> (f elem) ^ (if acc <> "" then del else " ") ^ acc) "" arr

let delim_comma f =
  delim f ", "

let rec string_of_expr _ = "<expr> "

and string_of_func_def _ = "<funcdef> "

and string_of_stmt stmt = match stmt with
| IfStatement {clauses, else_body} -> 
  (foldl (fun acc {cond, body} -> 
    acc
    ^ (if acc == "" then "if " else "elseif ") 
    ^ string_of_expr cond
    ^ "then "
    ^ string_of_stmt_list body
    ^ " "
  ) "" clauses)
  ^ match else_body with
  | Some body -> "else " ^ string_of_stmt_list body
  | None -> ""
  ^ "end "
| WhileStatement {cond, body} ->
  "while " ^ string_of_expr cond ^ "do " ^ string_of_stmt_list body ^ "end "
| DoStatement {body} ->
  "do " ^ string_of_stmt_list body ^ "end "
| NumericForStatement {var, start, finish, step, body} ->
  "for " ^ var ^ " = " ^ string_of_expr start ^ ", " ^ string_of_expr finish
  ^ (match step with
  | Some e -> ", " ^ string_of_expr e
  | None -> "")
  ^ "do " ^ string_of_stmt_list body ^ "end "
| GenericForStatement {var_list, generators, body} ->
  "for "
  ^ (delim_comma (fun x -> x) var_list) ^ "in "
  ^ (delim_comma string_of_expr generators) ^ "do "
  ^ string_of_stmt_list body ^ "end "
| RepeatStatement {cond, body} ->
  "repeat " ^ string_of_stmt_list body ^ "until " ^ string_of_expr cond
| FunctionStatement {name, is_local, f} ->
  (if is_local then "local " else "") ^ "function " ^ name ^ (string_of_func_def f)
| LocalStatement {names, init_exprs} ->
  "local " ^ (delim_comma (fun x -> x) names) ^ "= " ^ (delim_comma string_of_expr init_exprs)
| LabelStatement {label} -> "::" ^ label ^ "::"
| ReturnStatement {args} ->
  "return " ^ (delim_comma string_of_expr args)
| BreakStatement -> "break "
| GotoStatement {label} -> "goto " ^ label
| AssignmentStatement {lhs, rhs} ->
  string_of_expr lhs ^ "= " ^ string_of_expr rhs
| CallStatement {base, args} ->
  string_of_expr base ^ "(" ^ (delim_comma string_of_expr args) ^ ")"
| Eof -> ""

and string_of_stmt_list stmt_list = foldl (fun acc stmt -> acc ^ "\n" ^ (string_of_stmt stmt)) "" stmt_list

let _ =
  let ast = parse_file "kdb.lua" in
  iter (fun x -> put_line (string_of_stmt x)) ast
