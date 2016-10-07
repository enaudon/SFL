type binop =
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo

let tp_env =
  let tp = Type.Function (
    Type.Integer,
    Type.Function (Type.Integer, Type.Integer)
  ) in
  List.fold_left
    (fun acc id -> Ident.Map.add (Ident.of_string id) tp acc)
    (Ident.Map.empty)
    ["+"; "-"; "*"; "/"; "%"]

let binop_to_string op = match op with
  | Addition -> "+"
  | Subtraction -> "-"
  | Multiplication -> "*"
  | Division -> "/"
  | Modulo -> "%"

let binop_of_string str = match str with
  | "+" -> Addition
  | "-" -> Subtraction
  | "*" -> Multiplication
  | "/" -> Division
  | "%" -> Modulo
  | _ ->
    failwith (Printf.sprintf
        "Primative.binop_of_string: invalid binop \"%s\""
        str
    )
