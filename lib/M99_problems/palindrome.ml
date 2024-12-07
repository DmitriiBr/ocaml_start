(* Find out whether a list is a palindrome. Hint: A palindrome is its own reverse. *)
let list = [ "x"; "a"; "m"; "a"; "x" ]

(* Solution 2. Better*)
let is_palindrome list = List.rev list = list
