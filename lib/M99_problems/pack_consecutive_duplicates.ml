(* Pack consecutive duplicates of list elements into sublists. *)
let list =
  [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]

(* Solution *)
let pack list =
  let rec aux acc matrix = function
    | [] -> []
    | [ x ] -> (x :: acc) :: matrix
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: acc) matrix t
        else aux [] ((a :: acc) :: matrix) t
  in
  List.rev (aux [] [] list)
