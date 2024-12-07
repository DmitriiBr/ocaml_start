(* Find the last two (last and penultimate) elements of a list. *)
let list = [ "a"; "b"; "c"; "d" ]

(* Solution *)
let rec fn list =
  match list with
  | [] -> None
  | [ _elem ] -> None
  | [ first; second ] -> Some (first, second)
  | _ :: rest -> fn rest
