(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
module IR = Internal_rep

let flag_dump_cst = ref false
let flag_dump_ast = ref false
let flag_dump_tast = ref false
let flag_dump_ir = ref false
let flag_dump_llvm = ref false

let cmd_options = [
  ("--dump-cst", Arg.Set flag_dump_cst, "print the concrete syntax");
  ("--dump-ast", Arg.Set flag_dump_ast, "print the abstract syntax");
  ("--dump-tast", Arg.Set flag_dump_tast, "print the typed abstract syntax");
  ("--dump-ir", Arg.Set flag_dump_ir, "print the internal rep");
  ("--dump-llvm", Arg.Set flag_dump_llvm, "print the LLVM IR");
]

let usage_msg = Printf.sprintf "Usage:\n%s [options]" Sys.argv.(0)

let anon_arg_fun arg =
  Printf.printf "%s: ignoring option `%s'\n" Sys.argv.(0) arg

let parse s =
  List.rev (Parser.top_list Lexer.token (Lexing.from_string s))

let rec repl () =

  Arg.parse cmd_options anon_arg_fun usage_msg;

  print_string "? ";
  let input = read_line () in
  if input = "" then () else

  try
    let pt = parse input in
    if !flag_dump_cst then
      Printf.printf ">> Parse Tree:\n%s\n"
        (String.concat "" (List.map PT.top_to_string pt));

    let ast = Pt_ast_trans.f pt in
    if !flag_dump_ast then
      Printf.printf ">> Abstract Syntax Tree:\n%s\n"
        (String.concat "\n" (List.map AST.exp_to_string ast));

    let t_ast = Infer.infer ast in
    if !flag_dump_tast then
      Printf.printf ">> Typed Abstract Syntax Tree:\n%s\n"
          (String.concat "\n" (List.map AST.exp_to_string t_ast));

    let ids = AST.to_ident_list t_ast in
    let tps = AST.to_type_list Primative.tp_env t_ast in
    let vs = Ast_value_trans.f t_ast in

    let rec helper ids tps vs = match ids, tps, vs with
      | id :: id_tl, tp :: tp_tl, v :: v_tl ->
        Printf.printf "%s : %s = %s\n"
          (Ident.to_string id)
          (Type.to_string tp)
          (Value.to_string v);
        helper id_tl tp_tl v_tl
      | [], [], [] -> ()
      | _ -> failwith "evaluation error!"
    in
    helper ids tps vs;

    let ir = List.map Ast_ir_trans.f t_ast in
    if !flag_dump_ir then
      Printf.printf ">> Internal Rep:\n%s"
        (String.concat "" (List.map IR.top_to_string ir));

    let ll = Ir_llvm_trans.f ir in
    if !flag_dump_llvm
      then Printf.printf ">> LLVM:\n%s" (Llvm.string_of_llmodule ll);

    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()

let _ = repl()
