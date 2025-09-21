open Ast
open Utils

module Ast = Ast
module Error_msg = Error_msg

module Errors = struct
  let unknown_var x pos =
    Error_msg.mk pos (Format.asprintf "unknown variable `%s`" x)

  let not_implemented pos =
    Error_msg.mk pos "not implemented"

  let exp_ty t1 t2 pos =
    let msg =
      let open Ast in
      Format.asprintf
        "expected %a, got %a"
        pp_type t1 pp_type t2
    in Error_msg.mk pos msg

  let missing_main =
    Error_msg.mk dummy_pos "`main` function found"

  let dummy =
    Error_msg.mk dummy_pos "dummy"
end

open Errors

type context = ty Map.t
type store = value Map.t

let rec ty_expr (ctxt : context) (e : pos expr) : (ty, Error_msg.t) result =
  match e.expr with
  | Unit -> Ok UnitTy
  | Int32 _ -> Ok Int32Ty
  | PlaceExpr (Var x) -> (
      match Map.find_opt x ctxt with
      | None -> Error (unknown_var x e.meta)
      | Some ty -> Ok ty
    )
  | Bop (Add, e1, e2) ->
    let* t1 = ty_expr ctxt e1 in
    let* _ = guard (t1 = Int32Ty) (exp_ty Int32Ty t1 e1.meta) in
    let* t2 = ty_expr ctxt e2 in
    let* _ = guard (t2 = Int32Ty) (exp_ty Int32Ty t2 e2.meta) in
    Ok Int32Ty
  | _ -> Error (not_implemented e.meta)

let ty_stmt (ctxt : context) (stmt : pos stmt) : (context, Error_msg.t) result =
  match stmt.stmt with
  | Let (Immutable, x, e) ->
    let* ty = ty_expr ctxt e in
    Ok (Map.add x ty ctxt)
  | _ -> Error (not_implemented stmt.meta)

let ty_stmts (ctxt : context) (stmts : pos stmts) : (ty, Error_msg.t) result =
  let rec go ctxt stmts =
    match stmts with
    | [] -> Ok ctxt
    | s :: stmts ->
      let* ctxt = ty_stmt ctxt s in
      go ctxt stmts
  in
  let* ctxt = go ctxt stmts.stmts in
  ty_expr ctxt stmts.last

let ty (ctxt : context) (prog : pos prog) : (ty, Error_msg.t) result =
  let* stmts =
    match prog with
    | [] -> Error missing_main
    | [f] ->
      if f.name = "main"
      then Ok f.body
      else Error missing_main
    | fs ->
      match List.find_opt (fun f -> f.name <> "main") fs with
      | None -> Error missing_main
      | Some f -> Error (not_implemented f.meta)
  in
  ty_stmts ctxt stmts

let rec eval_expr (store : store) (e : pos expr) : value =
  match e.expr with
  | Unit -> UnitV
  | Int32 n -> Int32V n
  | PlaceExpr (Var x) -> Map.find x store
  | Bop(Add, e1, e2) -> (
    match eval_expr store e1, eval_expr store e2 with
    | Int32V m, Int32V n -> Int32V (Int32.add m n)
    | _ -> assert false
  )
  | _ -> assert false

let eval_stmt (store : store) (stmt : pos stmt) : store =
  match stmt.stmt with
  | Let (Immutable, x, e) -> Map.add x (eval_expr store e) store
  | _ -> assert false

let eval_stmts (store : store) (stmts : pos stmts) : value =
  eval_expr
    (List.fold_left eval_stmt store stmts.stmts)
    stmts.last

let eval (store : store) (prog : pos prog) : value =
  match prog with
  | [f] when f.name = "main" ->
    eval_stmts store f.body
  | _ -> assert false

let interp ~filename =
  let _ = Printf.printf "parsing...\n" in
  let* p = Parse.parse filename in
  let _ = Printf.printf "type checking...\n" in
  let* _ = ty Map.empty p in
  let _ = Printf.printf "evaluating...\n" in
  Ok (eval Map.empty p)
