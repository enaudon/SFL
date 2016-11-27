(*
 * Parse Tree
 *)

type id = string

type lit =
  | Boolean of bool
  | Integer of int
  | Tuple of exp list

and exp =
  | Variable of id
  | Literal of lit
  | BinaryOperation of Primative.binop * exp * exp
  | Application of exp * exp
  | Abstraction of id * exp
  | Binding of (id * exp) list * exp

type top =
  | Declaration of exp * exp
  | Expression of exp

let rec format_list pp_sep fmt ff = function
  | [] -> ()
  | [v] -> Format.fprintf ff "%a" fmt v
  | v :: tl ->
    Format.fprintf ff "%a%a%a"
      fmt v
      pp_sep ()
      (format_list pp_sep fmt) tl

let rec format_lit ff l = match l with
  | Boolean b -> Format.fprintf ff "%b" b
  | Integer i -> Format.fprintf ff "%i" i
  | Tuple es ->
    let pp_sep ff () = Format.fprintf ff ",@ " in
    Format.fprintf ff "(%a)" (format_list pp_sep format_exp) es

and format_exp ff exp =
  let paren ff e = match e with
    | Variable _
    | Literal _ -> format_exp ff e
    | _ -> Format.fprintf ff "(%a)" format_exp e
  in
  match exp with
    | Variable id -> Format.fprintf ff "%s" id
    | Literal l -> format_lit ff l
    | BinaryOperation (op, lhs, rhs) ->
      Format.fprintf ff "@[<hov 2>%a %s@ %a@]"
        format_exp lhs
        (Primative.binop_to_string op)
        format_exp rhs
    | Application _ ->
      let rec format_app ff exp = match exp with
        | Application (fn, arg) ->
          Format.fprintf ff "%a@ %a" format_app fn paren arg
        | _ -> paren ff exp
      in
      Format.fprintf ff "@[<hv 2>%a@]" format_app exp
    (* TODO: fix odd box opening/closing here--deets in comments *)
    | Abstraction _ ->
      let rec format_abs ff exp = match exp with
        | Abstraction (arg, body) ->
          let formatter = match body with
            | Abstraction _ -> Format.fprintf ff "%s ->@ %a"
            | _ -> Format.fprintf ff "%s ->%a"
          in
          formatter arg format_abs body
        (* The unmatched close closes the box below *)
        | _ -> Format.fprintf ff "@]@ %a" format_exp exp
      in
      (* The second box here (hov 2) is closed in the %a.  See above. *)
      Format.fprintf ff "@[<hv 2>@[<hov 2>%a@]" format_abs exp
    | Binding (binds, exp) ->
      let format_bind ff (id, value) =
        Format.fprintf ff "@[<hv 2>%s =@ %a@]" id format_exp value
      in
      let pp_sep ff () = Format.fprintf ff ";@ " in
      Format.fprintf ff
        "@[<hv 0>@[<hv 0>@[<hv 2>let@ %a@]@ in@]@ %a@]"
        (format_list pp_sep format_bind) binds
        format_exp exp

let format_top ff top = match top with
  | Declaration (exp1, exp2) ->
    Format.fprintf ff "@[<hv 2>%a =@ %a@]"
      format_exp exp1
      format_exp exp2
  | Expression exp ->
    Format.fprintf ff "@[<hv 2>%a@]"
      format_exp exp

let lit_to_string = Format.asprintf "%a" format_lit
let exp_to_string = Format.asprintf "%a" format_exp
let top_to_string = Format.asprintf "%a" format_top
