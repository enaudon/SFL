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
%token PLUS MINUS ASTERIK FSLASH PERCENT
%token COMMA
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
%left PLUS MINUS
%left ASTERIK FSLASH PERCENT

%start main
%type <Parse_tree.t> main

%%

main:
  | exp EOL                 { $1 }
  | error                   { failwith (error "expected expression" 1) }
;

literal:
  | INTEGER           { PT.Integer $1 }
  | BOOLEAN           { PT.Boolean $1 }
  | tuple             { PT.Tuple $1 }
;

tuple:
  | LPAREN exp_comma_list RPAREN  { List.rev $2 }
;

exp_comma_list:
  | exp                       { [$1] }
  | exp_comma_list COMMA exp  { $3 :: $1 }
;

exp:
  | literal           { PT.Literal ($1) }
  | VAR               { PT.Variable $1 }
  | exp PLUS exp      { PT.BinaryOperation (PT.Addition, $1, $3) }
  | exp MINUS exp     { PT.BinaryOperation (PT.Subtraction, $1, $3) }
  | exp ASTERIK exp   { PT.BinaryOperation (PT.Multiplication, $1, $3) }
  | exp FSLASH exp    { PT.BinaryOperation (PT.Division, $1, $3) }
  | exp PERCENT exp   { PT.BinaryOperation (PT.Modulo, $1, $3) }
  | VAR IMP LPAREN exp RPAREN       { PT.Abstraction ($1, $4) }
  | exp LPAREN exp RPAREN           { PT.Application ($1, $3) }   /* S/R */
  | LET exp EQUAL exp IN exp END    { PT.Declaration ($2, $4, $6) }
  | LPAREN exp RPAREN               { $2 }
;
