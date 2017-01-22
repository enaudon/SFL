val format_list :
  (Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a list ->
  unit
