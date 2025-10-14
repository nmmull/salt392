open Utils
open Ast

type ty =
  | Int32Ty
  | UnitTy
  | BorrowTy of ident

type slot =
  {
    ty: ty;
    mut: mut;
  }

type context = slot Map.t

let rec pp_type (ctxt : context) ppf =
  let open Format in
  function
  | Int32Ty -> fprintf ppf "i32"
  | UnitTy -> fprintf ppf "()"
  | BorrowTy x -> fprintf ppf "&%a" (pp_type ctxt) (Map.find x ctxt).ty

module Errors = struct
  let exp_ty pos ctxt t1 t2 =
    let msg =
      Format.asprintf
        "expected %a, got %a"
        (pp_type ctxt) t1 (pp_type ctxt) t2
    in Error_msg.mk pos msg

  let cannot_deref pos ctxt ty =
    Error_msg.mk pos (Format.asprintf "type `%a` cannot be dereferenced" (pp_type ctxt) ty)

  let cannot_find_var pos x =
    Error_msg.mk pos (Format.asprintf "cannot find `%s` in this scope" x)

  let invalid_assign pos =
    Error_msg.mk pos "invalid left-hand side of assignment"

  let cannot_assign pos x =
    Error_msg.mk pos (Format.asprintf "cannot assign twice to immutable variable `%s`" x)

  let cannot_assign_borrowed pos x =
    Error_msg.mk pos (Format.asprintf "cannot assign to `%s` because it is borrowed" x)

  let cannot_shadow pos x =
    Error_msg.mk pos (Format.asprintf "cannot shadow variable `%s`" x)
end
open Errors

let ty_var pos ctxt x =
  match Map.find_opt x ctxt with
  | Some slot -> Ok slot
  | None -> Error (cannot_find_var pos x)

let rec ty_place pos ctxt = function
  | Var x -> ty_var pos ctxt x
  | Deref w1 -> (
      let* slot = ty_place pos ctxt w1 in
      match slot.ty with
      | BorrowTy x -> ty_var pos ctxt x
      | ty -> Error (cannot_deref pos ctxt ty)
    )

let rec ty_equiv ctxt t1 t2 =
   match t1, t2 with
  | _ when t1 = t2 -> true
  | BorrowTy x, BorrowTy y -> (
    let t1 = (Map.find x ctxt).ty in
    let t2 = (Map.find y ctxt).ty in
    ty_equiv ctxt t1 t2
  )
  | _ -> false

let writable ctxt x =
  let has_conflict _ slot =
    match slot.ty with
    | BorrowTy y -> x = y
    | _ -> false
  in not (Map.exists has_conflict ctxt)

let rec ty_expr ctxt e =
  match e.expr with
  | Unit -> Ok (UnitTy, ctxt)
  | Int32 _ -> Ok (Int32Ty, ctxt)
  | Place_expr w -> Result.map (fun slot -> slot.ty, ctxt) (ty_place e.pos ctxt w)
  | ImmBorrow w -> (
    match w with
    | Var x ->
      let* _ = ty_var e.pos ctxt x in
      Ok (BorrowTy x, ctxt)
    | Deref w -> (
      let* slot = ty_place e.pos ctxt w in
      match slot.ty with
      | BorrowTy y -> Ok (BorrowTy y, ctxt)
      | ty -> Error (cannot_deref e.pos ctxt ty)
    )
  )
  | Assign (e1, e2) ->
    match e1.expr with
    | Place_expr (Var x) -> (
        let* slot = ty_var e.pos ctxt x in
        let* _ = guard (slot.mut = Mutable) (cannot_assign e1.pos x) in
        let t1 = slot.ty in
        let* (t2, ctxt) = ty_expr ctxt e2 in
        let* _ = guard (ty_equiv ctxt t1 t2) (exp_ty e2.pos ctxt t1 t2) in
        let* _ = guard (writable ctxt x) (cannot_assign_borrowed e1.pos x) in
        Ok (UnitTy, Map.add x { ty = t2 ; mut = Mutable } ctxt)
      )
    | _ -> Error (invalid_assign e1.pos)

let ty_stmt ctxt stmt =
  match stmt.stmt with
  | Expr e -> Result.map snd (ty_expr ctxt e)
  | Let (mut, x, e) ->
    if Map.mem x ctxt
    then Error (cannot_shadow stmt.pos x)
    else
      let* ty, ctxt = ty_expr ctxt e in
      Ok (Map.add x {ty; mut} ctxt)

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
  | Loc of ident

type store = value Map.t

let pp_value ppf =
  let open Format in
  function
  | UnitV -> fprintf ppf "()"
  | Int32V n -> fprintf ppf "%d" (Int32.to_int n)
  | Loc x -> fprintf ppf "loc(%s)" x

let rec loc store w =
  match w with
  | Var x -> x
  | Deref w ->
    match Map.find (loc store w) store with
    | Loc x -> x
    | _ -> assert false

let rec eval_expr store e =
  match e.expr with
  | Unit -> store, UnitV
  | Int32 n -> store, Int32V n
  | Place_expr w -> store, Map.find (loc store w) store
  | ImmBorrow w -> store, Loc (loc store w)
  | Assign (e1, e2) ->
    match e1.expr, eval_expr store e2 with
    | Place_expr (Var x), (store, v) -> Map.add x v store, UnitV
    | _ -> assert false

let eval_stmt store s =
  match s.stmt with
  | Expr e -> fst (eval_expr store e)
  | Let (_, x, e) ->
    let store, v = eval_expr store e in
    Map.add x v store

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
  let _ = printf "%a\n" pp_value (snd (eval Map.empty p)) in
  Ok ()
