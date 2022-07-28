type var = string
type expr =
  | Variable of { name : var }
  | Abstraction of { param : var; body : expr; }
  | Application of { abstraction : expr; argument : expr }
  | Integer of int
  | BinOp of (binop * expr * expr)

and binop =
  | Plus