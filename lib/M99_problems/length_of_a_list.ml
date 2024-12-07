(* Find the number of elements of a list. *)
let list = [ "a"; "b"; "c"; "d" ]

(* Solution *)
let fn list =
  let rec loop list count =
    match list with [] -> count | _ :: rest -> loop rest (count + 1)
  in
  loop list 0
