type row_col = {
  row : int ;
  col : int ;
}

type t = {
  file : string ;
  start : row_col ;
  finish : row_col ;
}

let create f (sr, sc) (er, ec) =
  {
    file = f ;
    start = { row = sr; col = sc; } ;
    finish = { row = er; col = ec; } ;
  }

let file {file; _} = file
let start {start = {row; col}; _} = row, col
let finish {finish = {row; col}; _} = row, col
