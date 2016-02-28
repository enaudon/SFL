%{
module PT = Parse_tree
open Lexing

let error msg nterm =
  let start = Parsing.rhs_start_pos nterm in
  let finish = Parsing.rhs_end_pos nterm in
  Printf.sprintf "%s (%d,%d - %d,%d): %s"
    start.pos_fname
    start.pos_lnum start.pos_cnum
    finish.pos_lnum finish.pos_cnum
    msg

%}

%token <string> VAR
%token LPAREN RPAREN
%token IMP APP
%token EOL
%token EQUAL
%token LET IN END
%token <bool> BOOLEAN
%token <int> INTEGER

%nonassoc FUN
%nonassoc VAR LPAREN
%left APP

%start main
%type <Parse_tree.t> main

%%

main:
  | exp EOL                 { $1 }
  | error                   { failwith (error "expected expression" 1) }
;

exp:
  | INTEGER                         { PT.Literal (PT.Integer $1) }
  | BOOLEAN                         { PT.Literal (PT.Boolean $1) }
  | VAR                             { PT.Variable $1 }
  | VAR IMP exp %prec FUN           { PT.Abstraction ($1, $3) }
  | exp exp %prec APP               { PT.Application ($1, $2) }   /* S/R */
  | LET exp EQUAL exp IN exp END    { PT.Declaration ($2, $4, $6) }
  | LPAREN exp RPAREN               { $2 }
;
