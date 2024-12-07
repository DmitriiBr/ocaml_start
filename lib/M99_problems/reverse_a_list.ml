(* Reverse a list. OCaml standard library has List.rev but we ask that you reimplement it. *)
let list = [ "a"; "b"; "c"; "d" ]

(* Solution *)
let reverse list =
  let rec aux list new_list =
    match list with
    | [] -> new_list
    | elem :: rest -> aux rest new_list @ [ elem ]
  in
  aux list []

(* Solution 2 with @*)
let reverse list =
  let rec aux list new_list =
    match list with
    | [] -> new_list
    | elem :: rest -> aux rest (elem :: new_list)
  in
  aux list []
