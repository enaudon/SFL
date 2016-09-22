(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
module AST2 = Abs_syntax_tree2
module TT = Type_tree
module IR = Internal_rep
module LL = Llvm_trans


let parse s =
  let top_list = Parser.top_list Lexer.token (Lexing.from_string s) in
  List.rev top_list

let rec repl () =
  print_string "? ";
  let input = read_line () in
  if input = "" then () else
  try
    let pt = parse input in
    Printf.printf ">> Parse Tree:\n%s"
      (String.concat "" (List.map PT.top_to_string pt));

    (* Old pathway *)
    let tt = Infer.infer pt in
    Printf.printf ">> Abs Syntax Tree I:\n%s\n"
      (String.concat "\n" (List.map AST.exp_to_string tt));
    Printf.printf ">> Type:\n%s\n"
      (String.concat "\n"
        (List.map TT.typo_to_string (List.map TT.to_typo tt)));
    let ir_old = List.map IR.top_of_tt tt in
    Printf.printf ">> Internal Rep I:\n%s"
      (String.concat "" (List.map IR.top_to_string ir_old));
    let ll_old = LL.translate ir_old in
    Printf.printf ">> LLVM I:\n%s" (Llvm.string_of_llmodule ll_old);

    (* New pathway *)
    let ast = PT.top_to_ast pt in
    Printf.printf ">> Abs Syntax Tree II:\n%s\n"
      (String.concat "\n" (List.map AST2.exp_to_string ast));
    let ast' = Infer2.infer ast in
    Printf.printf "  ----\n%s\n"
      (String.concat "\n" (List.map AST2.exp_to_string ast'));
    let ir_new = List.map IR.top_of_ast ast' in
    Printf.printf ">> Internal Rep II:\n%s"
      (String.concat "" (List.map IR.top_to_string ir_new));
    assert (ir_new = ir_old);
    let ll_new = LL.translate ir_new in
    Printf.printf ">> LLVM II:\n%s" (Llvm.string_of_llmodule ll_new);

    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()

let _ = repl()
