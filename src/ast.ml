type var = string
type expr =
  | Variable of { name : var }
  | Abstraction of (var * Types.typ * expr)
  | Application of { abstraction : expr; argument : expr }
  | Integer of int
  | Boolean of bool
  | Let of (var * expr * expr)
