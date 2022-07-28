%{
open Ast

(* f a b c -> (((f a) b) c) *)
let rec make_application e = function
  | [] -> failwith "precondition violated"
  | [e'] -> Application { abstraction = e; argument = e' }
  | e' :: es -> make_application (Application { abstraction = e; argument = e'}) es
%}

%token <int> INT
%token <string> IDENT
%token TRUE
%token FALSE
%token FUN
%token ARROW
%token LPARENS
%token RPARENS
%token LET
%token EQUALS
%token IN
%token PLUS
%token EOF

%start <Ast.expr> prog

%%

let prog :=
  | e = expr; EOF; { e }

let expr :=
  | sub_expr
  | abstraction
  | application
  | binop
  | let_expr

let sub_expr ==
  | terminal
  | LPARENS; e=expr; RPARENS; { e } 

let terminal ==
  | var = IDENT; { Variable { name = var } }
  | i = INT; { Integer i }
  | boolean

let boolean ==
  | TRUE; { Boolean true }
  | FALSE; { Boolean false }

let let_expr ==
  | LET; name = IDENT; EQUALS; e1 = expr; IN; e2 = expr; { Let (name, e1, e2) }

let abstraction ==
  | FUN; param = IDENT; ARROW; e = expr; { Abstraction { param = param; body = e } }

let application :=
  | e = sub_expr; es = sub_expr+; { make_application e es }

let binop ==
  | e1 = expr; PLUS; e2 = expr; { BinOp (Plus, e1, e2) }
