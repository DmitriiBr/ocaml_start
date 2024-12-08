(* Flatten a nested list structure. *)

type node_nesting = One | Many
type 'a node = One of 'a | Many of 'a node list

let list : string node list =
  [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]

(* Solution *)
let flatten_1 (node_list : string node list) =
  let rec aux acc node_list =
    match node_list with
    | [] -> acc
    | elem :: rest -> (
        match elem with
        | One elem -> elem :: aux acc rest
        | Many elem -> aux (aux acc rest) elem)
  in
  aux [] node_list

(* Solution 2*)
let flatten_2 list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list)
