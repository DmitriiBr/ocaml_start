let divide x y = if y == 3 then None else Some (x / y)
let x = 40

let pattern_matching () =
  for i = 1 to 10 do
    match divide x i with
    | None -> print_endline ("Cannot divide on: " ^ Int.to_string i)
    | Some value -> print_endline (Int.to_string value)
  done

