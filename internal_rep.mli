type id = string

type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

type lit =
  | Boolean of bool
  | Integer of int

type exp =
  | Literal of lit
  | Variable of id
  | BinaryOperation of binop * exp * exp
  | Application of id * exp list
  | Binding of id * exp * exp

type top =
  | VariableDecl of id * exp
  | FunctionDecl of id * id list * exp * Type_tree.typo
  | Expression of exp

val exp_of_tt : Type_tree.exp -> exp
val top_of_tt : Type_tree.exp -> top

val lit_to_string : lit -> string
val exp_to_string : exp -> string
val top_to_string : top -> string
