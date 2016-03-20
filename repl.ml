(*
 *  Interpreter loop
 *)


module PT = Parse_tree
module TT = Type_tree


let parse (s : string) : PT.t =
  Parser.main Lexer.token (Lexing.from_string s)

let rec repl () =
  print_string "? ";
  let input = read_line () in
  if input = "" then () else
  try
    let pt = parse input in
    Printf.printf "%s\n" (PT.to_string pt);
    let tt = Infer.infer pt in
    Printf.printf "%s\n" (TT.typo_to_string (TT.to_typo tt));
    repl ()
  with
    | Failure msg -> print_endline msg; repl ()
    | Parsing.Parse_error -> repl ()

let _ = repl()