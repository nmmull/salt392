open Utils

type ident = string

type mut = Main_parser.Ast.mut = Mutable | Immutable

type place_expr = Main_parser.Ast.place_expr =
  | Var of ident
  | Deref of place_expr

type _expr =
  | Unit
  | Int32 of int32
  | Place_expr of place_expr
  | ImmBorrow of place_expr
  | Assign of expr * expr
and expr = { expr : _expr ; pos : pos }

type _stmt =
  | Expr of expr
  | Let of mut * ident * expr
and stmt = { stmt : _stmt ; pos : pos }

type prog = { stmts : stmt list ; last : expr }
