(* Find the N'th element of a list. *)
let list = [ "a"; "b"; "c"; "d" ]

(* Solution *)
let rec fn list k =
  match list with
  | [] -> None
  | elem :: rest -> if k = 0 then Some elem else fn rest (k - 1)
