
type pos = Lexing.position * Lexing.position

type ident = string

type _expr =
  | Unit
  | Int32 of int32
  | Var of ident
  | Add of expr * expr
and expr =
  {
    expr : _expr;
    pos : pos;
  }

type _stmt =
  | LetImm of ident * expr
and stmt =
  {
    stmt : _stmt;
    pos : pos;
  }

type prog =
  {
    stmts : stmt list;
    last : expr;
  }
