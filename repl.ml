(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module AST = Abs_syntax_tree
module TT = Type_tree
module IR = Internal_rep
module LL = Llvm_trans


let parse s =
  Parser.top Lexer.token (Lexing.from_string s)

let rec repl () =
  print_string "? ";
  let input = read_line () in
  if input = "" then () else
  try
    let pt = parse input in
    Printf.printf ">> Parse Tree: %s" (PT.top_to_string pt);
    let tt = Infer.infer pt in
    Printf.printf ">> Abs Syntax Tree: %s" (AST.top_to_string tt);
    Printf.printf ">> Type: %s\n" (TT.typo_to_string (TT.to_typo tt));
    let ir = IR.top_of_tt tt in
    Printf.printf ">> IR: %s" (IR.top_to_string ir);
    repl ()
  with
    | Failure msg ->  Printf.printf "%s\n" msg; repl ()
    | Parsing.Parse_error -> Printf.printf "Parser error\n"; repl ()
    | _ -> Printf.printf "Unknown error\n"; repl ()

let _ = repl()
