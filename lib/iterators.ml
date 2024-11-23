let my_list = [1; 2; 3; 4;]
let f elem = Printf.printf "I'm looking at elemnt %d now\n" elem

(* It is best solution for looping over lists *)
let print_list list = List.iter f list

(* That is how you can map over list *)
let map_list = print_list (List.map (fun el -> el * 2) my_list)

(* That is how you can sum all of elements with folding them left, same as JS Array.reduce *)
let folded_left_list = Printf.printf "Sum of all elements: %d" (List.fold_left ( + ) 1 my_list)

(* That is how you can iter ofer string with printf *)
let string_iter = String.iter (fun ch -> (Printf.printf "%c\n" ch)) "Hello world"

