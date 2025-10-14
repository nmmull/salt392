open Utils

module A = Parse.Ast
open Ast

let not_implemented pos =
  Error_msg.mk pos "not implemented"

let missing_main =
  Error_msg.mk dummy_pos "`main` function found"

let rec expr_of_expr (e : pos A.expr) : (expr, Error_msg.t) result =
  let _expr_of_expr : pos A._expr -> (_expr, Error_msg.t) result = function
    | Unit -> Ok Unit
    | Int32 n -> Ok (Int32 n)
    | Place_expr (Var x) -> Ok (Var x)
    | Bop(Add, e1, e2) ->
      let* e1 = expr_of_expr e1 in
      let* e2 = expr_of_expr e2 in
      Ok (Add (e1, e2))
    | _ -> Error (not_implemented e.meta)
  in
  let* expr = _expr_of_expr e.expr in
  Ok { expr ; pos = e.meta }

let stmt_of_stmt (s : pos A.stmt) : (stmt, Error_msg.t) result =
  let _stmt_of_stmt : pos A._stmt -> (_stmt, Error_msg.t) result = function
    | Let (Immutable, x, e) ->
      let* e = expr_of_expr e in
      Ok (LetImm (x, e))
    | _ -> Error (not_implemented s.meta)
  in
  let* stmt = _stmt_of_stmt s.stmt in
  Ok { stmt ; pos = s.meta }

let prog_of_stmts (ss : pos A.stmts) : (prog, Error_msg.t) result =
  let* stmts = all_ok (List.map stmt_of_stmt ss.stmts) in
  let* last = expr_of_expr ss.last in
  Ok { stmts ; last }

let parse ~filename =
  let open A in
  let* p = Parse.parse ~filename in
  match p with
  | [] -> Error missing_main
  | [f] ->
    if f.name = "main"
    && f.args = []
    && f.out_ty = UnitTy
    then prog_of_stmts f.body
    else Error missing_main
  | fs ->
    match List.find_opt (fun f -> f.name <> "main") fs with
    | None -> Error missing_main
    | Some f -> Error (not_implemented f.meta)
