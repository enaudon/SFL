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

let format_lit ff l = match l with
  | Boolean b -> Format.fprintf ff "%b" b
  | Integer i -> Format.fprintf ff "%i" i

let rec format_exp ff exp = match exp with
  | Literal l -> format_lit ff l
  | Variable id -> Format.fprintf ff "%s" (Ident.to_string id)
  | BinaryOperation (op, lhs, rhs) ->
    Format.fprintf ff "@[<hov 2>%a %s@ %a@]"
      format_exp lhs
      (Primative.binop_to_string op)
      format_exp rhs
  | Binding (id, value, exp) ->
    Format.fprintf ff
      "@[<hv 0>@[<hv 0>@[<hv 2>let %s =@ %a@]@ in@]@ %a@]"
      (Ident.to_string id)
      format_exp value
      format_exp exp
  | Application (id, args) ->
    let format_app ff arg = Format.fprintf ff "%a" format_exp arg in
    let pp_sep ff () = Format.fprintf ff ",@ " in
    Format.fprintf ff "@[<hv 2>%s(@,%a@]@,)"
      (Ident.to_string id)
      (Format_util.format_list pp_sep format_app) args

let format_top ff top = match top with
  | VariableDecl (id, exp) ->
    Format.fprintf ff "@[<hv 2>%s =@ %a@]\n"
      (Ident.to_string id)
      format_exp exp
  | FunctionDecl (id, args, body, tp) ->
    let format_app ff arg =
      Format.fprintf ff "%s" (Ident.to_string arg)
    in
    let pp_sep ff () = Format.fprintf ff ",@ " in
    Format.fprintf ff "@[<hv 2>%s : %s (%a) = %a@]\n"
      (Ident.to_string id)
      (Type.to_string tp)
      (Format_util.format_list pp_sep format_app) args
      format_exp body
  | Expression (exp) ->
    Format.fprintf ff "%a\n" format_exp exp

let lit_to_string = Format.asprintf "%a" format_lit
let exp_to_string = Format.asprintf "%a" format_exp
let top_to_string = Format.asprintf "%a" format_top
