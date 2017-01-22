(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
module V = Value
module IR = Internal_rep


let parse s =
  let top_list = Parser.top_list Lexer.token (Lexing.from_string s) in
  List.rev top_list

let rec repl () =
  print_string "? ";
  let input = read_line () in
  if input = "" then () else
  try
    let pt = parse input in
    Printf.printf ">> Parse Tree:\n%s\n"
      (String.concat "" (List.map PT.top_to_string pt));

    let ast = Pt_ast_trans.f pt in
    Printf.printf ">> Abs Syntax Tree:\n%s\n"
      (String.concat "\n" (List.map AST.exp_to_string ast));

    let ast' = Infer.infer ast in
    Printf.printf "  ----\n%s\n"
      (String.concat "\n" (List.map AST.exp_to_string ast'));

    let ir = List.map Ast_ir_trans.f ast' in
    Printf.printf ">> Internal Rep:\n%s"
      (String.concat "" (List.map IR.top_to_string ir));

    let v = List.map Ast_value_trans.f ast' in
    Printf.printf ">> Value:\n%s\n"
      (String.concat "" (List.map V.to_string v));

    let ll = Ir_llvm_trans.f ir in
    Printf.printf ">> LLVM:\n%s" (Llvm.string_of_llmodule ll);

    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()

let _ = repl()
