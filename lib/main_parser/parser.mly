%{
    open Ast
    open Ast.Expr
    open Ast.Stmt
%}

%token<string> IDENT
%token<int32> INT
%token TRUE "true" FALSE "false"
%token INTTY "i32" BOOLTY "bool"
%token FN "fn"
%token LET "let" MUT "mut"
%token IF "if" ELSE "else"
%token BOX "Box::new" ASSERT "assert!"
%token LPAREN "(" RPAREN ")"
%token LBRACKET "{" RBRACKET "}"
%token COMMA "," DOT "."
%token COLON ":" SEMICOLON ";"
%token AMPERSAND "&"
%token STAR "*"
%token ARROW "->"
%token EQ "="
%token ADD "+" SUB "-" DIV "/" MOD "%"
%token BEQ "==" NEQ "!="
%token LT "<" LTE "<=" GT ">" GTE ">="
%token OR "||" AND "&&" NOT "!"
%token ADDEQ "+=" SUBEQ "-="
%token MULEQ "*=" DIVEQ "/=" MODEQ "%="
%token EOF

%right EQ ADDEQ SUBEQ MULEQ DIVEQ MODEQ
%left OR
%left AND
%nonassoc NOT
%left BEQ NEQ LT LTE GT GTE
%left ADD SUB
%left STAR DIV MOD

%start <(Lexing.position * Lexing.position) Ast.prog> main

%%

nonempty_comma_list(x):
  | x=x { [x] }
  | x=x "," { [x] }
  | x=x "," xs=nonempty_comma_list(x) { x :: xs }

comma_list(x):
  | es=nonempty_comma_list(x)? { Option.value es ~default:[] }

main:
  | fns=fn* EOF { fns }

ty:
  | "i32" { Int32Ty }
  | "bool" { BoolTy }
  | "(" ")" { UnitTy }
  | "&" m="mut"? ty=ty
    {
      let mut =
	match m with
	| None -> Immutable
	| Some _ -> Mutable
      in
      BorrowTy (mut, ty)
    }
  | "(" ty=ty "," tys=nonempty_comma_list(ty) ")" { TupleTy (ty :: tys) }

fn:
  | "fn" name=IDENT "(" args=comma_list(type_annot(IDENT)) ")" out_ty=out_ty body=block
    { {name;args;out_ty;body;meta=$loc} }

type_annot(x):
  | x=x ":" ty=ty { x, ty }

%inline out_ty:
  | { UnitTy }
  | "->" ty=ty { ty }

stmt:
  | "let" m="mut"? x=IDENT "=" e=expr ";"
    {
      let mut =
	match m with
	| None -> Immutable
	| Some _ -> Mutable
      in let_ $loc mut x e
    }
  | e=expr ";" { expr $loc e }
  | e=expr_block { expr $loc e }

stmts:
  | s=stmt ss=stmts { Stmts.mk (s :: ss.stmts) ss.last }
  | s=stmt { Stmts.mk [s] (unit $loc) }
  | expr=expr_no_block { Stmts.mk [] expr }

block:
  | "{" stmts=stmts? "}" { Option.value stmts ~default:(Stmts.mk [] (unit $loc)) }

expr:
  | e=expr_block { e }
  | e=expr_no_block { e }

expr_block:
  | b=block { block $loc b }
  | "if" e=expr b1=block { if_ $loc e b1 (Stmts.mk [] (unit $loc)) (* dummy location *) }
  | "if" e=expr b1=block "else" b2=block { if_ $loc e b1 b2 }

expr_no_block:
  | n=INT { int32 $loc n }
  | "(" ")" { unit $loc }
  | "true" { true_ $loc }
  | "false" { false_ $loc }
  | w=place_expr { place_expr $loc w }
  | w=place_expr "." i=INT { tuple_access $loc w i }
  | "&" m="mut"? w=place_expr
    {
      let mut =
	match m with
	| None -> Immutable
	| Some _ -> Mutable
      in
      borrow $loc mut w
    }
  | "Box::new" "(" e=expr ")" { box $loc e }
  | "assert!" "(" e=expr ")" { assert_ $loc e }
  | e1=expr_no_block op=bop e2=expr_no_block { bop $loc op e1 e2 }
  | op=uop e=expr_no_block { uop $loc op e }
  | f=IDENT "(" args=nonempty_comma_list(expr) ")" { call $loc f args }
  | "(" e=expr "," es=nonempty_comma_list(expr) ")" { tuple $loc (e :: es) }
  | "(" e=expr ")" { Expr.mk $loc e.expr }

%inline uop:
  | "-" { Neg }
  | "!" { Not }

%inline bop:
  | "||" { Or }
  | "&&" { And }
  | "==" { Eq }
  | "!=" { Neq }
  | "<" { Lt }
  | "<=" { Lte }
  | ">" { Gt }
  | ">=" { Gte }
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }
  | "%" { Mod }
  | "=" { Asn }
  | "+=" { AddAsn }
  | "-=" { SubAsn }
  | "*=" { MulAsn }
  | "/=" { DivAsn }
  | "%=" { ModAsn }

place_expr:
  | x=IDENT { Var x }
  | "*" e=place_expr { Deref e }
