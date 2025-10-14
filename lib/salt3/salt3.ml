open Utils
open Ast

type ty =
  | Int32Ty
  | UnitTy
  | BorrowTy of mut * ident

type partial_ty =
  | Full of ty
  | Moved of ty

type lifetime = int

let static_lftm = 0

type slot =
  {
    ty: partial_ty;
    mut: mut;
    lftm : lifetime;
  }

type context = slot Map.t

let rec pp_type (ctxt : context) ppf =
  let open Format in
  function
  | Full Int32Ty -> fprintf ppf "i32"
  | Full UnitTy -> fprintf ppf "()"
  | Full BorrowTy (mut, x) -> (
    let ty = (Map.find x ctxt).ty in
    match mut with
    | Mutable -> fprintf ppf "&mut %a" (pp_type ctxt) ty
    | Immutable -> fprintf ppf "&%a" (pp_type ctxt) ty
  )
  | Moved ty -> fprintf ppf "%a" (pp_type ctxt) (Full ty)

module Errors = struct
  let exp_ty pos ctxt t1 t2 =
    let msg =
      Format.asprintf
        "expected %a, got %a"
        (pp_type ctxt) t1 (pp_type ctxt) t2
    in Error_msg.mk pos msg

  let unknown_var pos x =
    Error_msg.mk pos
      (Format.asprintf "unknown variable `%s`" x)

  let cannot_deref pos ctxt ty =
    Error_msg.mk pos
      (Format.asprintf "type `%a` cannot be dereferenced" (pp_type ctxt) ty)

  let cannot_find_var pos x =
    Error_msg.mk pos
      (Format.asprintf "cannot find `%s` in this scope" x)

  let invalid_assign pos =
    Error_msg.mk pos
      "invalid left-hand side of assignment"

  let cannot_assign_twice pos w =
    Error_msg.mk pos
      (Format.asprintf "cannot assign twice to immutable variable `%a`" pp_place_expr w)

  let cannot_assign_borrowed pos w =
    Error_msg.mk pos
      (Format.asprintf "cannot assign to `%a` because it is borrowed" pp_place_expr w)

  let cannot_shadow pos x =
    Error_msg.mk pos
      (Format.asprintf "cannot shadow variable `%s`" x)

  let use_moved pos w =
    Error_msg.mk pos
      (Format.asprintf "use of moved value: `%a`" pp_place_expr w)

  let borrow_moved pos w =
    Error_msg.mk pos
      (Format.asprintf "borrow of moved value: `%a`" pp_place_expr w)

  let cannot_borrow_imm pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as immutable because it is also borrowed as mutable"
         pp_place_expr w)

  let cannot_borrow_mut pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as immutable because it is also borrowed as immutable"
         pp_place_expr w)

  let cannot_borrow_mut_behind_ref pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as mutable because, asis behind a `&` reference"
         pp_place_expr w)

  let cannot_assign_behind_ref pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot assign to `%a`, which is behind a `&` reference"
         pp_place_expr w)

  let cannot_borrow_mut_not_mut pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as mutable, as it is not declared as mutable"
         pp_place_expr w)

  let not_lives pos =
    Error_msg.mk pos
      (Format.asprintf "borrowed value does not live long enough")
end
open Errors

let copy = function
  | UnitTy | Int32Ty | BorrowTy (Immutable, _) -> true
  | _ -> false

let ty_var pos ctxt x =
  match Map.find_opt x ctxt with
  | Some slot -> Ok slot
  | None -> Error (unknown_var pos x)

let rec ty_place pos ctxt = function
  | Var x -> ty_var pos ctxt x
  | Deref w1 -> (
      let* slot = ty_place pos ctxt w1 in
      match slot.ty with
      | Full BorrowTy (_, x)-> ty_var pos ctxt x
      | Full ty -> Error (cannot_deref pos ctxt (Full ty))
      | Moved _ -> Error (use_moved pos w1)
    )

let var_of_place ctxt = function
  | Var x -> x
  | Deref w ->
    match ty_place dummy_pos ctxt w with
    | Ok { ty = Full BorrowTy (_, y) ; _} -> y
    | _ -> assert false

let rec base_var = function
  | Var x -> x
  | Deref w -> base_var w

let conflict w x = base_var w = x

let writable ctxt w =
  Map.for_all
    (fun _ slot ->
       match slot.ty with
       | Full (BorrowTy (Immutable, x)) -> not (conflict w x)
       | _ -> true)
    ctxt

let readable ctxt w =
  Map.for_all
    (fun _ slot ->
       match slot.ty with
       | Full (BorrowTy (_, x)) -> not (conflict w x)
       | _ -> true)
    ctxt

let move ctxt w =
  let x = base_var w in
  let slot = Map.find x ctxt in
  match slot.ty with
  | Full t -> Map.add x { slot with ty = Moved t } ctxt
  | _ -> assert false

let rec nest_place w x =
  match w with
  | Var _ -> Var x
  | Deref w1 -> Deref (nest_place w1 x)

let rec muttable ctxt w ty =
  match w, ty with
  | Var _, _ -> true
  | Deref w, BorrowTy(Mutable, x) -> mutt ctxt (nest_place w x)
  | _ -> false
and mutt ctxt w =
  match (Map.find (base_var w) ctxt).ty with
  | Full ty -> muttable ctxt w ty
  | _ -> false

let rec ty_equiv ctxt t1 t2 =
   match t1, t2 with
  | _ when t1 = t2 -> true
  | Full BorrowTy (m1, x), Full BorrowTy (m2, y) when m1 = m2 -> (
    let t1 = (Map.find x ctxt).ty in
    let t2 = (Map.find y ctxt).ty in
    ty_equiv ctxt t1 t2
  )
  | Moved t1, t2 -> ty_equiv ctxt (Full t1) t2
  | t1, Moved t2 -> ty_equiv ctxt t1 (Full t2)
  | _ -> false

let rec write pos ctxt w t =
  let update ctxt w t1 =
    match w, t1 with
    | Var _, _ -> Ok ctxt
    | Deref w, BorrowTy (Mutable, x) -> write pos ctxt (nest_place w x) t
    | Deref w, BorrowTy (Immutable, _) -> Error (cannot_assign_behind_ref pos w)
    | _ -> assert false
  in
  let x = base_var w in
  let slot = Map.find x ctxt in
  match slot.ty with
  | Full ty ->
    let* ctxt = update ctxt w ty in
    Ok (Map.add x { slot with ty = Full t } ctxt)
  | _ -> assert false

let lftm_lives l1 l2 = l1 <= l2

let ty_lives ctxt ty lftm =
  match ty with
  | Int32Ty -> true
  | UnitTy -> true
  | BorrowTy (_, x) -> lftm_lives (Map.find x ctxt).lftm lftm

let drop ctxt lftm =
  let not_lftm _ s = s.lftm <> lftm in
  Map.filter not_lftm ctxt

let rec ty_expr lftm ctxt e =
  match e.expr with
  | Unit -> Ok (UnitTy, ctxt)
  | Int32 _ -> Ok (Int32Ty, ctxt)
  | Place_expr w -> (
    let* slot = ty_place e.pos ctxt w in
    match slot.ty with
    | Full ty ->
      if copy ty
      then Ok (ty, ctxt)
      else Ok (ty, move ctxt w)
    | Moved _ -> Error (use_moved e.pos w)
  )
  | Borrow (Immutable, w) -> (
      let* slot = ty_place e.pos ctxt w in
      let* _ = guard (readable ctxt w) (cannot_borrow_imm e.pos w) in
      match slot.ty with
      | Full _ -> Ok (BorrowTy (Immutable, var_of_place ctxt w), ctxt)
      | Moved _ -> Error (borrow_moved e.pos w)
    )
  | Borrow (Mutable, w) -> (
      let* slot = ty_place e.pos ctxt w in
      let* _ = guard (slot.mut = Mutable) (cannot_borrow_mut_not_mut e.pos w) in
      let* _ = guard (mutt ctxt w) (cannot_borrow_mut_behind_ref e.pos w) in
      let* _ = guard (writable ctxt w) (cannot_borrow_mut e.pos w) in
      match slot.ty with
      | Full _ -> Ok (BorrowTy (Mutable, var_of_place ctxt w), ctxt)
      | Moved _ -> Error (borrow_moved e.pos w)
    )
  | Assign (e1, e2) -> (
    match e1.expr with
    | Place_expr w -> (
        let* slot = Result.map_error (fun _ -> use_moved e.pos w) (ty_place e1.pos ctxt w) in
        let* _ = guard (slot.mut = Mutable) (cannot_assign_twice e.pos w) in
        let t1 = slot.ty in
        let* t2, ctxt = ty_expr lftm ctxt e2 in
        let* _ = guard (ty_equiv ctxt t1 (Full t2)) (exp_ty e2.pos ctxt t1 (Full t2)) in
        let* _ = guard (ty_lives ctxt t2 slot.lftm) (not_lives e2.pos) in
        let* ctxt = write e.pos ctxt w t2 in
        let* _ = guard (writable ctxt w) (cannot_assign_borrowed e.pos w) in
        Ok (UnitTy, ctxt)
      )
    | _ -> Error (invalid_assign e1.pos)
  )
  | Block p ->
    let* t, ctxt = ty (lftm + 1) ctxt p in
    let* _ = guard (ty_lives ctxt t lftm) (not_lives p.last.pos) in
    Ok (t, drop ctxt (lftm + 1))

and ty_stmt lftm ctxt stmt =
  match stmt.stmt with
  | Expr e -> Result.map snd (ty_expr lftm ctxt e)
  | Let (mut, x, e) ->
    if Map.mem x ctxt
    then Error (cannot_shadow stmt.pos x)
    else
      let* ty, ctxt = ty_expr lftm ctxt e in
      Ok (Map.add x {ty = Full ty; mut; lftm} ctxt)

and ty lftm ctxt prog =
  let rec go ctxt stmts =
    match stmts with
    | [] -> Ok ctxt
    | s :: stmts ->
      let* ctxt = ty_stmt lftm ctxt s in
      go ctxt stmts
  in
  let* ctxt = go ctxt prog.stmts in
  ty_expr lftm ctxt prog.last

type owned = Owned | Unowned

type value =
  | UnitV
  | Int32V of int32
  | Loc of mut * owned * ident

type slot_value = {
  value : value option;
  lftm : lifetime;
}

type store = slot_value Map.t

let pp_value ppf =
  let open Format in
  function
  | UnitV -> fprintf ppf "()"
  | Int32V n -> fprintf ppf "%d" (Int32.to_int n)
  | Loc (_, Owned, x) -> fprintf ppf "oloc(%s)" x
  | Loc (_, Unowned, x) -> fprintf ppf "uloc(%s)" x

let rec loc store w =
  match w with
  | Var x -> x
  | Deref w ->
    match (Map.find (loc store w) store).value with
    | Some Loc (_, _, x) -> x
    | _ -> assert false

let read store w = Map.find (loc store w) store
let write store w v =
  let x = loc store w in
  let slot = Map.find x store in
  Map.add x {slot with value = v } store

let rec drop_vals store vs =
  match vs with
  | [] -> store
  | Some Loc (_, Owned, x) :: vs ->
    drop_vals
      (Map.remove x store)
      ((Map.find x store).value :: vs)
  | _ -> drop_vals store vs

let drop_lftm store lftm =
  let lftm_locs (x, slot) =
    if slot.lftm = lftm
    then Some (Some (Loc (Mutable, Owned, x)))
    else None
  in
  let vals =
    store
    |> Map.to_list
    |> List.filter_map lftm_locs
  in
  drop_vals store vals

let rec eval_expr lftm store e =
  match e.expr with
  | Unit -> store, UnitV
  | Int32 n -> store, Int32V n
  | Place_expr w -> (
      let slot = read store w in
      match slot.value with
      | Some Loc (Mutable, owned, x) -> Map.add x { slot with value = None } store, Loc (Mutable, owned, x)
      | Some v -> store, v
      | _ -> assert false
  )
  | Borrow (mut, w) -> store, Loc (mut, Unowned, loc store w)
  | Assign (e1, e2) -> (
    match e1.expr, eval_expr lftm store e2 with
    | Place_expr w, (store, v) -> write store w (Some v), UnitV
    | _ -> assert false
  )
  | Block p ->
    let store, v = eval (lftm + 1) store p in
    drop_lftm store (lftm + 1), v

and eval_stmt lftm store s =
  match s.stmt with
  | Expr e -> fst (eval_expr lftm store e)
  | Let (_, x, e) ->
    let store, v = eval_expr lftm store e in
    Map.add x { value = Some v ; lftm } store

and eval lftm store prog =
  eval_expr lftm
    (List.fold_left (eval_stmt lftm) store prog.stmts)
    prog.last

let interp ~filename =
  let open Format in
  let _ = printf "parsing...\n" in
  let* p = Parser.parse ~filename in
  let _ = printf "type checking...\n" in
  let* _ = ty static_lftm Map.empty p in
  let _ = printf "evaluating...\n" in
  let _ = printf "%a\n" pp_value (snd (eval 0 Map.empty p)) in
  Ok ()
