(* Determine Whether a Given Integer Number Is Prime *)
(* Solution *)

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n > 1 && is_not_divisor 2

(* Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
(* SHIT! Not needed, but has been created =( *)
let find_simple_divisors = function
  | 0 -> []
  | n ->
      let rec find_div acc n d =
        if d = n then d :: acc
        else if n mod d = 0 then d :: find_div acc (n / d) 2
        else find_div acc n (d + 1)
      in
      find_div [] n 2

(* Solution *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
