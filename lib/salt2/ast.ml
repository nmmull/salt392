open Utils

type ident = string

type mut = Parse.Ast.mut = Mutable | Immutable

type place_expr = Parse.Ast.place_expr =
  | Var of ident
  | Deref of place_expr

let rec pp_place_expr ppf =
  let open Format in
  function
  | Var x -> fprintf ppf "%s" x
  | Deref w -> fprintf ppf "*%a" pp_place_expr w

type _expr =
  | Unit
  | Int32 of int32
  | Place_expr of place_expr
  | Borrow of mut * place_expr
  | Assign of expr * expr
and expr = { expr : _expr ; pos : pos }

type _stmt =
  | Expr of expr
  | Let of mut * ident * expr
and stmt = { stmt : _stmt ; pos : pos }

type prog = { stmts : stmt list ; last : expr }
