open Ast
open Utils

module Ast = Ast
module Error_msg = Error_msg

module Errors = struct
  let exp_ty t1 t2 pos =
    let msg =
      let open Ast in
      Format.asprintf
        "expected %a, got %a"
        pp_type t1 pp_type t2
    in Error_msg.mk pos msg

  let unknown_var x pos =
    Error_msg.mk pos (Format.asprintf "unknown variable `%s`" x)

  let cannot_deref pos ty =
    Error_msg.mk pos (Format.asprintf "type `%a` cannot be dereferenced" Ast.pp_type ty)

  let cannot_assign pos x =
    Error_msg.mk pos (Format.asprintf "cannot assign twice to immutable variable `%s`" x)

  let invalid_assign pos =
    Error_msg.mk pos "invalid left-hand side of assignment"

  let not_implemented pos = Error_msg.mk pos "not implemented"

  let dummy = Error_msg.mk dummy_pos "dummy"

  let missing_main = Error_msg.mk dummy_pos "`main` function found"
end

type slot =
  {
    ty: ty;
    mut: mut;
  }

type context = slot Map.t
type store = value Map.t

(* let ty_equiv (t1 : ty) (t2 : ty) : bool = assert false *)

(* let writable (ctxt : context) (x : ident) : bool = assert false *)

(* let ty_expr (ctxt : context) (e : pos expr) : (ty * context, Error_msg.t) result = assert false *)

(* let ty_stmt (_ctxt : context) (_stmt : pos stmt) : (context, Error_msg.t) result = assert false *)

(* let ty_stmts (_ctxt : context) (_stmts : pos stmts) : (ty * context, Error_msg.t) result = assert false *)

let ty (_ctxt : context) (_prog : pos prog) : (ty * context, Error_msg.t) result = assert false

(* let rec loc (_store : store) (_w : place_expr) : ident = assert false *)

(* let read (_store : store) (_w : place_expr) : value = assert false *)

(* let eval_expr (_store : store) (_e : pos expr) : store * value = assert false *)

(* let eval_stmt (_store : store) (_stmt : pos stmt) : store = assert false *)

(* let eval_stmts (_store : store) (_stmts : pos stmts) : store * value = assert false *)

let eval (_store : store) (_prog : pos prog) : store * value = assert false

let interp ~filename =
  let _ = Printf.printf "parsing...\n" in
  let* p = Parse.parse filename in
  let _ = Printf.printf "type checking...\n" in
  let* _ = ty Map.empty p in
  let _ = Printf.printf "evaluating...\n" in
  Ok (eval Map.empty p)
