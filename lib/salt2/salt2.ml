open Utils
include Ast

type ty =
  | Int32Ty
  | UnitTy
  | BorrowTy of mut * place_expr

type partial_ty =
  | Full of ty
  | Moved of ty

type slot =
  {
    ty: partial_ty;
    mut: mut;
  }

type context = slot Map.t

type sty =
  | Int32TyS
  | UnitTyS
  | BorrowTyS of mut * sty

let rec sty_place ctxt = function
  | Var x -> (
      match (Map.find x ctxt).ty with
      | Full ty -> sty_of_ty ctxt ty
      | Moved ty -> sty_of_ty ctxt ty
    )
  | Deref w -> (
      match sty_place ctxt w with
      | BorrowTyS (_, ty) -> ty
      | _ -> assert false
    )
and sty_of_ty ctxt = function
  | Int32Ty -> Int32TyS
  | UnitTy -> UnitTyS
  | BorrowTy (m, w) -> BorrowTyS (m, sty_place ctxt w)

let rec pp_sty ppf =
  let open Format in
  function
  | Int32TyS -> fprintf ppf "i32"
  | UnitTyS -> fprintf ppf "()"
  | BorrowTyS (mut, ty) -> (
      match mut with
      | Mutable -> fprintf ppf "&mut %a" pp_sty ty
      | Immutable -> fprintf ppf "&%a" pp_sty ty
    )

let sty_of_partial_ty ctxt = function
  | Full ty -> sty_of_ty ctxt ty
  | Moved ty -> sty_of_ty ctxt ty

let pp_partial_ty ctxt = Fmt.using (sty_of_partial_ty ctxt) pp_sty

module Errors = struct
  let exp_ty pos ctxt t1 t2 =
    let msg =
      Format.asprintf
        "expected %a, got %a"
        (pp_partial_ty ctxt) t1 (pp_partial_ty ctxt) t2
    in Error_msg.mk pos msg

  let unknown_var pos x =
    Error_msg.mk pos
      (Format.asprintf "unknown variable `%s`" x)

  let cannot_deref pos ctxt ty =
    Error_msg.mk pos
      (Format.asprintf "type `%a` cannot be dereferenced" (pp_partial_ty ctxt) ty)

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

  let cannot_borrow_mut_twice pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as mutable more than once at a time"
         pp_place_expr w)

  let cannot_borrow_mut pos w =
    Error_msg.mk
      ~message2:"mutable borrow occurs here"
      pos
      (Format.asprintf
         "cannot borrow `%a` as mutable because it is also borrowed as immutable"
         pp_place_expr w)

  let cannot_borrow_mut_behind_ref pos w =
    Error_msg.mk pos
      (Format.asprintf
         "cannot borrow `%a` as mutable because, as it is behind a `&` reference"
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
      | Full BorrowTy (_, w2)-> ty_place pos ctxt w2
      | Full ty -> Error (cannot_deref pos ctxt (Full ty))
      | Moved _ -> Error (use_moved pos w1)
    )

let rec base_var = function
  | Var x -> x
  | Deref w -> base_var w

let writable ctxt w1 =
  let has_conflict _ slot =
    match slot.ty with
    | Full (BorrowTy (_, w2)) -> w1 = w2
    | _ -> false
  in
  not (Map.exists has_conflict ctxt)

let readable ctxt w1 =
  let has_conflict _ slot =
    match slot.ty with
    | Full (BorrowTy (Mutable, w2)) -> w1 = w2
    | _ -> false
  in
  not (Map.exists has_conflict ctxt)

let move ctxt w =
  let x = base_var w in
  let slot = Map.find x ctxt in
  match slot.ty with
  | Full t -> Map.add x { slot with ty = Moved t } ctxt
  | _ -> assert false

let nest_place w1 w2 =
  let rec go acc = function
    | Var _ -> acc
    | Deref w1 -> go (Deref acc) w1
  in go w2 w1

let rec mutable_ ctxt = function
  | Var x ->
    let slot = Result.get_ok (ty_var dummy_pos ctxt x) in
    slot.mut = Mutable
  | Deref w1 -> (
      let x = base_var w1 in
      match (Result.get_ok (ty_var dummy_pos ctxt x)).ty with
      | Full BorrowTy (Mutable, w2) -> mutable_ ctxt (nest_place w1 w2)
      | _ -> false
    )

let rec ty_equiv ctxt t1 t2 =
   match t1, t2 with
  | _ when t1 = t2 -> true
  | Full BorrowTy (m1, w1), Full BorrowTy (m2, w2) when m1 = m2 -> (
      let slot1 = ty_place dummy_pos ctxt w1 in
      let slot2 = ty_place dummy_pos ctxt w2 in
      match slot1, slot2 with
      | Ok slot1, Ok slot2 -> ty_equiv ctxt slot1.ty slot2.ty
      | _ -> false
  )
  | Moved t1, t2 -> ty_equiv ctxt (Full t1) t2
  | t1, Moved t2 -> ty_equiv ctxt t1 (Full t2)
  | _ -> false

let rec write pos ctxt w t =
  match w with
  | Var x ->
      let* slot = ty_var pos ctxt x in
      Ok (Map.add x { slot with ty = Full t } ctxt)
  | Deref w1 ->
    let x = base_var w1 in
    let* slot = ty_var pos ctxt x in
    match slot.ty with
    | Full BorrowTy (Mutable, w2) -> write pos ctxt (nest_place w1 w2) t
    | _ -> Error (cannot_assign_behind_ref pos w)

let replace t1 w t2 =
  let rec extend w1 w2 w3 =
    match w1 with
    | Var x -> Var x
    | Deref w1 ->
      if w1 = w2
      then w3
      else Deref (extend w1 w2 w3)
  in
  match t1, t2 with
  | BorrowTy (m, w1), BorrowTy (_, w2) ->
    BorrowTy (m, extend w1 w w2)
  | _ -> t1

let replace t1 w t2 =
  match t1 with
  | Full t1 -> Full (replace t1 w t2)
  | Moved t1 -> Moved (replace t1 w t2)

let update pos ctxt w t =
  Result.map
    (Map.map (fun slot -> { slot with ty = replace slot.ty w t }))
    (write pos ctxt w t)

let rec ty_expr ctxt e =
  match e.expr with
  | Unit -> Ok (UnitTy, ctxt)
  | Int32 _ -> Ok (Int32Ty, ctxt)
  | Place_expr (copyable, w) -> (
    let* slot = ty_place e.pos ctxt w in
    match slot.ty with
    | Full ty ->
      if copy ty
      then begin
        copyable := true;
        Ok (ty, ctxt)
      end
      else Ok (ty, move ctxt w)
    | Moved _ -> Error (use_moved e.pos w)
  )
  | Borrow (Immutable, Var x) -> (
      let* slot = ty_place e.pos ctxt (Var x) in
      let* _ = guard (readable ctxt (Var x)) (cannot_borrow_imm e.pos (Var x)) in
      match slot.ty with
      | Full _ -> Ok (BorrowTy (Immutable, Var x), ctxt)
      | Moved _ -> Error (borrow_moved e.pos (Var x))
    )
  | Borrow (Immutable, w1) -> (
      let* _ = guard (readable ctxt w1) (cannot_borrow_imm e.pos w1) in
      match w1 with
      | Var x ->
        let* _ = ty_place e.pos ctxt w1 in
        Ok (BorrowTy (Immutable, Var x), ctxt)
      | Deref w1 -> (
        let* slot = ty_place e.pos ctxt w1 in
        match slot.ty with
        | Full BorrowTy (Immutable, w2) -> Ok (BorrowTy (Immutable, w2), ctxt)
        | Full BorrowTy (Mutable, _) -> Ok (BorrowTy (Immutable, Deref w1), ctxt)
        | Full ty -> Error (cannot_deref e.pos ctxt (Full ty))
        | Moved _ -> Error (borrow_moved e.pos w1)
      )
    )
  | Borrow (Mutable, w) -> (
      let* slot = ty_place e.pos ctxt w in
      let* _ = guard (slot.mut = Mutable) (cannot_borrow_mut_not_mut e.pos w) in
      let* _ = guard (mutable_ ctxt w) (cannot_borrow_mut_behind_ref e.pos w) in
      let writable = writable ctxt w in
      let readable = readable ctxt w in
      let* _ =
        guard writable
          (if not readable
           then cannot_borrow_mut_twice e.pos w
           else cannot_borrow_mut e.pos w)
      in
      match slot.ty with
      | Full _ -> Ok (BorrowTy (Mutable, w), ctxt)
      | Moved _ -> Error (borrow_moved e.pos w)
    )
  | Assign (e1, e2) ->
    match e1.expr with
    | Place_expr (_, w) -> (
        let* slot = Result.map_error (fun _ -> use_moved e.pos w) (ty_place e1.pos ctxt w) in
        let* _ = guard (slot.mut = Mutable) (cannot_assign_twice e.pos w) in
        let t1 = slot.ty in
        let* t2, ctxt = ty_expr ctxt e2 in
        let* _ = guard (ty_equiv ctxt t1 (Full t2)) (exp_ty e2.pos ctxt t1 (Full t2)) in
        let* ctxt = update e.pos ctxt w t2 in
        let* _ = guard (writable ctxt w) (cannot_assign_borrowed e.pos w) in
        Ok (UnitTy, ctxt)
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
      Ok (Map.add x {ty = Full ty; mut} ctxt)

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

type store = value option Map.t

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
    | Some Loc x -> x
    | _ -> assert false

let read store w = Map.find (loc store w) store
let write store w v = Map.add (loc store w) v store

let rec eval_expr store e =
  match e.expr with
  | Unit -> store, UnitV
  | Int32 n -> store, Int32V n
  | Place_expr (copyable, w) -> (
      match read store w with
      | Some Loc x ->
        if !copyable
        then store, Loc x
        else Map.add x None store, Loc x
      | Some v -> store, v
      | _ -> assert false
  )
  | Borrow (_, w) -> store, Loc (loc store w)
  | Assign (e1, e2) ->
    match e1.expr, eval_expr store e2 with
    | Place_expr (_, w), (store, v) -> write store w (Some v), UnitV
    | _ -> assert false

let eval_stmt store s =
  match s.stmt with
  | Expr e -> fst (eval_expr store e)
  | Let (_, x, e) ->
    let store, v = eval_expr store e in
    Map.add x (Some v) store

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
