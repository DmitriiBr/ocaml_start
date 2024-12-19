(*List *)
let list =
  [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

(*Run-Length Encoding*)
(*Solution*)
let encode list =
  let rec aux acc count list =
    match list with
    | [] -> acc
    | [ x ] -> (count, x) :: acc
    | a :: (b :: _ as rest) ->
        if a = b then aux acc (count + 1) rest
        else aux ((count, a) :: acc) 1 rest
  in
  List.rev (aux [] 1 list)

(*Modified Run-Length Encoding*)
(*Solution (Using previous encode funtion to solve this)*)
type 'a rle = One of 'a | Many of int * 'a

let m_encode list =
  List.map
    (fun x -> match x with 1, a -> One a | a, b -> Many (a, b))
    (encode list)

(*Decode a Run-Length Encoded List*)
(*Solution*)
let encoded_list =
  [
    Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e");
  ]

let decode_list list =
  let rec spread_tuple acc count c =
    if count = 1 then c :: acc else c :: spread_tuple acc (count - 1) c
  in
  let rec aux acc list =
    match list with
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (x, y) :: t -> aux (spread_tuple acc x y) t
  in
  List.rev (aux [] list)
