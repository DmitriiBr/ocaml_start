(* Write a function last : 'a list -> 'a option that returns the last element of a list *)
let list = [ "a"; "b"; "c"; "d" ]

(* Solution *)
let rec fn list =
  match list with
  | [] -> None
  | [ elem ] -> Some elem
  | _ :: rest_list -> fn rest_list
