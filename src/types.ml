type typ =
  | TInteger
  | TBool

let type_to_string = function
  | TInteger -> "int"
  | TBool -> "boolean"
