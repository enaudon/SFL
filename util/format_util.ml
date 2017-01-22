let rec format_list pp_sep fmt ff = function
  | [] -> ()
  | v :: [] -> Format.fprintf ff "%a" fmt v
  | v :: tl ->
    Format.fprintf ff "%a%a%a"
      fmt v
      pp_sep ()
      (format_list pp_sep fmt) tl
