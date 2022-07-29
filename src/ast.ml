type var = string
type binop = Plus
type expr =
  | Variable of { name : var }
  | Abstraction of { param : var; body : expr; }
  | Application of { abstraction : expr; argument : expr }
  | Integer of int
  | Boolean of bool
  | BinOp of (binop * expr * expr)
  | Let of (var * expr * expr)
