type ident = string

type mut = Mutable | Immutable

type place_expr = Var of ident | Deref of place_expr

type ty =
  | Int32Ty | BoolTy | UnitTy
  | BorrowTy of mut * ty * place_expr list
  | TupleTy of ty list

type uop = Not | Neg

type bop =
  | Or | And
  | Eq | Neq | Lt | Lte | Gt | Gte
  | Add | Sub | Mul | Div | Mod
  | Asn | AddAsn | SubAsn | MulAsn | DivAsn | ModAsn

type 'a _expr =
  | Unit
  | Int32 of int32
  | Bool of bool
  | Uop of uop * 'a expr
  | Bop of bop * 'a expr * 'a expr
  | PlaceExpr of place_expr
  | Assert of 'a expr
  | Borrow of mut * place_expr
  | Box of 'a expr
  | Block of 'a stmts
  | If of 'a expr * 'a stmts * 'a stmts
  | Tuple of 'a expr list
  | TupleAccess of place_expr * int32
  | Call of ident * 'a expr list
and 'a _stmt =
  | Let of mut * ident * 'a expr
  | Expr of 'a expr
and 'a expr = { expr: 'a _expr; meta: 'a }
and 'a stmt = { stmt: 'a _stmt; meta: 'a }
and 'a stmts = { stmts: 'a stmt list; last: 'a expr }

module Expr = struct
  let mk meta expr = {meta; expr}

  let unit meta = mk meta Unit
  let int32 meta n = mk meta (Int32 n)
  let bool meta b = mk meta (Bool b)
  let true_ meta = bool meta true
  let false_ meta = bool meta false
  let uop meta op e = mk meta (Uop (op, e))
  let bop meta op e1 e2 = mk meta (Bop (op, e1, e2))
  let if_ meta e1 ss1 ss2 = mk meta (If (e1, ss1, ss2))
  let assert_ meta e = mk meta (Assert e)
  let place_expr meta w = mk meta (PlaceExpr w)
  let borrow meta mut w = mk meta (Borrow (mut, w))
  let tuple meta es = mk meta (Tuple es)
  let tuple_access meta w i = mk meta (TupleAccess (w, i))
  let call meta f es = mk meta (Call (f, es))
  let box meta e = mk meta (Box e)
  let block meta ss = mk meta (Block ss)
end

module Stmt = struct
  let mk meta stmt = {meta; stmt}

  let let_ meta mut x e = mk meta (Let (mut, x, e))
  let expr meta e = mk meta (Expr e)
end

module Stmts = struct
  let mk stmts last = {stmts; last}
  let is_empty ss = List.is_empty ss.stmts && ss.last.expr = Unit
end

type 'a fn =
  {
    name: ident;
    args: (ident * ty) list;
    out_ty : ty;
    body: 'a stmts;
    meta: 'a;
  }

type 'a prog = 'a fn list

type owned = Owned | Unowned

type value =
  | UnitV
  | BoolV of bool
  | Int32V of int32
  | Loc of owned * ident

let rec pp_type ppf =
  let open Format in
  function
  | Int32Ty -> fprintf ppf "i32"
  | BoolTy -> fprintf ppf "bool"
  | UnitTy -> fprintf ppf "()"
  | BorrowTy (mut, ty, _) -> (
      match mut with
      | Immutable -> fprintf ppf "&%a" pp_type ty
      | Mutable -> fprintf ppf "&mut %a" pp_type ty
    )
  | TupleTy tys ->
    let pp_sep ppf () = fprintf ppf ",@;<1 2>" in
    fprintf ppf "@[<hv>(@;<0 2>%a%t)@]"
      (pp_print_list ~pp_sep pp_type) tys
      (Format.pp_print_custom_break
         ~fits:("", 0, "")
         ~breaks:(",", 0, ""))

let pp_value ppf =
  let open Format in
  function
  | UnitV -> fprintf ppf "()"
  | BoolV b -> fprintf ppf "%b" b
  | Int32V n -> fprintf ppf "%d" (Int32.to_int n)
  | Loc (Owned, x) -> fprintf ppf "oloc(%s)" x
  | Loc (Unowned, x) -> fprintf ppf "uloc(%s)" x
