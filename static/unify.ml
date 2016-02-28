module Identifier = struct
  type t = int

  let next = ref 0
  let reset () = next := 0
  let fresh () =
    let i = !next in
    incr next;
    i

  type _t = t
  module S = struct
    type t = _t
    let compare = compare
  end

  module type SET = Set.S with type elt = t
  module Set = Set.Make(S)

  module type MAP = Map.S with type key = t
  module Map = Map.Make(S)
end

type term =
  | Variable of Identifier.t
  | Function of Identifier.t * term array
  | Disjunction of term list

module Substitution = struct
  module IdMap = Identifier.Map
  type t = term IdMap.t

  let identity = IdMap.empty
  let singleton id t = IdMap.singleton id t

  let rec apply s t = match t with
    | Variable (id) ->
      begin
        try IdMap.find id s
        with Not_found -> t
      end
    | Function (id, ts) ->
      let ts' = Array.map (apply s) ts in
      Function (id, ts')
    | Disjunction (ts) ->
      let ts' = List.map (apply s) ts in
      Disjunction (ts')

  let compose s1 s2 =
    let fold_fn id t acc = match apply s2 t with
      | Variable (jd) when id = jd -> acc
      | t' -> IdMap.add id t' acc
    in
    let filter_fn id t =
      try let _ = IdMap.find id s1 in false
      with Not_found -> true
    in
    let merge_fn id t1 t2 = match t1, t2 with
      | None, None -> failwith "Map.Make.merge bug"
      | None, Some t
      | Some t, None -> Some t
      | Some _, Some _ -> failwith "Unify.Substitution.compose bug"
    in
    let s1' = IdMap.fold fold_fn s1 IdMap.empty in
    let s2' = IdMap.filter filter_fn s2 in
    IdMap.merge merge_fn s1' s2'

end

module S = Substitution

let variable id = Variable (id)
let constant id =
  let hack = Array.create 0 (Variable id) in
  Function (id, hack)
let funktion (id, tms) = Function (id, tms)

let rec term_to_string tm = match tm with
  | Variable (id) ->
    Printf.sprintf "%d" id
  | Function (id, tms) ->
    let tms_strs = Array.to_list (Array.map term_to_string tms) in
    Printf.sprintf "%d(%s)" id (String.concat ", " tms_strs)
  | Disjunction (tms) ->
    let tms_strs = List.map term_to_string tms in
    Printf.sprintf "{%s}" (String.concat ", " tms_strs)

let constraint_to_string (tm1, tm2) =
  Printf.sprintf "%s = %s" (term_to_string tm1) (term_to_string tm2)

let apply_variable s t = match t with
  | Variable (_) -> S.apply s t
  | _ -> t

let unify e =
  let rec fn s e = match e with
    | [] -> s
    | (t1, t2) :: es ->
      let t1' = apply_variable s t1 in
      let t2' = apply_variable s t2 in
      match t1', t2' with
        | Variable (id1), (Variable (id2) as t) ->
          if id1 = id2
            then fn s es
            else
              let s' = S.compose s (S.singleton id1 t) in
              fn s' es
        | (Variable (id) as v), (Function (_) as t)
        | (Function (_) as t), (Variable (id) as v)
        | (Variable (id) as v), (Disjunction (_) as t)
        | (Disjunction (_) as t), (Variable (id) as v) ->
          if (* occurs v t *) false
            then failwith "Circularity"
            else
              let s' = S.compose s (S.singleton id t) in
              fn s' es
        | Function (id1, ts1), Function (id2, ts2) ->
          if id1 = id2 && Array.length ts1 = Array.length ts2
            then
              let rec f ts1 ts2 acc = match ts1, ts2 with
                | [], [] -> acc
                | hd1 :: tl1, hd2 :: tl2 ->
                  let acc' = ((hd1, hd2) :: acc) in
                  f tl1 tl2 acc'
                | _, _ -> failwith "Unify.unify bug"
              in
              let e' = f (Array.to_list ts1) (Array.to_list ts2) es in
              fn s e'
            else
              failwith "ununifiable"
        | Disjunction (ts1), Disjunction (ts2) ->
          failwith "cannot unify two disjunctions, yet"
  in
  let s = S.identity in
  fn s e
