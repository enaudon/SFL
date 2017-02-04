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

let dummy = create "" (-1, -1) (-1, -1)

let file {file; _} = file
let start {start = {row; col}; _} = row, col
let finish {finish = {row; col}; _} = row, col

let format ff {file; start; finish} =
  let file' = if file = "" then "_" else file in
  let {row = sr; col = sc} = start in
  let {row = fr; col = fc} = finish in
  if sr = fr
    then
      Format.fprintf ff "@[<hv 2>%s (%d,%d-%d)@]" file' sr sc fc
    else
      Format.fprintf ff "@[<hv 2>%s (%d,%d - %d,%d)@]" file' sr sc fr fc

let to_string = Format.asprintf "%a" format
