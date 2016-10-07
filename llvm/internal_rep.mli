(*
 *  Internal Representation
 *)


type id = Ident.t

type lit =
  | Boolean of bool
  | Integer of int

type exp =
  | Literal of lit
  | Variable of id
  | BinaryOperation of Primative.binop * exp * exp
  | Application of id * exp list
  | Binding of id * exp * exp

type top =
  | VariableDecl of id * exp
  | FunctionDecl of id * id list * exp * Type.t
  | Expression of exp

val lit_to_string : lit -> string
val exp_to_string : exp -> string
val top_to_string : top -> string
