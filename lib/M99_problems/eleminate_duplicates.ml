(* Eliminate consecutive duplicates of list elements. *)

let list =
  [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

(* Solution 1. Expected solution *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

(* Solution 2. My solution. This solution compresses ALL duplicates *)
let compress_2 list =
  let rec aux acc list =
    match list with
    | [] -> acc
    | e :: rest -> if List.mem e acc then aux acc rest else aux (e :: acc) rest
  in
  List.rev (aux [] list)
