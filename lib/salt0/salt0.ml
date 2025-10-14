open Utils
open Ast

type ty =
  | Int32Ty
  | UnitTy

type context = ty Map.t

let pp_type ppf =
  let open Format in
  function
  | Int32Ty -> fprintf ppf "i32"
  | UnitTy -> fprintf ppf "()"

module Errors = struct
  let exp_ty pos t1 t2 =
    let msg =
      Format.asprintf
        "expected %a, got %a"
        pp_type t1 pp_type t2
    in Error_msg.mk pos msg

  let cannot_find_var pos x =
    Error_msg.mk pos (Format.asprintf "cannot find `%s` in this scope" x)
end
open Errors

let rec ty_expr ctxt e =
  match e.expr with
  | Unit -> Ok UnitTy
  | Int32 _ -> Ok Int32Ty
  | Var x -> (
      match Map.find_opt x ctxt with
      | None -> Error (cannot_find_var e.pos x)
      | Some ty -> Ok ty
    )
  | Add (e1, e2) ->
    let* t1 = ty_expr ctxt e1 in
    let* _ = guard (t1 = Int32Ty) (exp_ty e1.pos Int32Ty t1) in
    let* t2 = ty_expr ctxt e2 in
    let* _ = guard (t2 = Int32Ty) (exp_ty e2.pos Int32Ty t2) in
    Ok Int32Ty

let ty_stmt ctxt stmt =
  match stmt.stmt with
  | LetImm (x, e) ->
    let* ty = ty_expr ctxt e in
    Ok (Map.add x ty ctxt)

let ty ctxt prog =
  let rec go ctxt stmts =
    match stmts with
    | [] -> Ok ctxt
    | s :: stmts ->
      let* ctxt = ty_stmt ctxt s in
      go ctxt stmts
  in
  let* ctxt = go ctxt prog.stmts in
  ty_expr ctxt prog.last

type value =
  | UnitV
  | Int32V of int32

type store = value Map.t

let pp_value ppf =
  let open Format in
  function
  | UnitV -> fprintf ppf "()"
  | Int32V n -> fprintf ppf "%d" (Int32.to_int n)

let rec eval_expr store e =
  match e.expr with
  | Unit -> UnitV
  | Int32 n -> Int32V n
  | Var x -> Map.find x store
  | Add (e1, e2) ->
    match eval_expr store e1, eval_expr store e2 with
    | Int32V m, Int32V n -> Int32V (Int32.add m n)
    | _ -> assert false

let eval_stmt store s =
  match s.stmt with
  | LetImm (x, e) -> Map.add x (eval_expr store e) store

let eval store prog =
  eval_expr
    (List.fold_left eval_stmt store prog.stmts)
    prog.last

let interp ~filename =
  let open Format in
  let _ = printf "parsing...\n" in
  let* p = Parser.parse ~filename in
  let _ = printf "type checking...\n" in
  let* _ = ty Map.empty p in
  let _ = printf "evaluating...\n" in
  let _ = printf "%a\n" pp_value (eval Map.empty p) in
  Ok ()
