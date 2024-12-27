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

(*Run-Length Encoding of a List (Direct Solution)*)
(*Solution*)
let encode_d list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> rle count x :: acc
    | x :: (y :: _ as t) ->
        if x = y then aux (count + 1) acc t else aux 0 (rle count x :: acc) t
  in
  List.rev (aux 0 [] list)

(*Duplicate the Elements of a List *)
(*Solution*)
let list_to_d = [ "a"; "b"; "c"; "c"; "d" ]
let rec duplicate = function [] -> [] | x :: t -> x :: x :: duplicate_2 t
let rec duplicate_2 = function [] -> [] | x :: t -> duplicate_2 t @ [ x; x ]

(* This is tail recursive variant*)
let duplicate_tail_rec list =
  let rec aux acc = function [] -> acc | x :: t -> aux (x :: x :: acc) t in
  aux [] (List.rev list)

(*Replicate the Elements of a List a Given Number of Times*)
let list_to_r = [ "a"; "b"; "c" ]

(*Basic solution*)
let rec create_replicates times acc x =
  if times = 0 then acc else create_replicates (times - 1) (x :: acc) x

let replicate list times =
  let rec aux acc i = function
    | [] -> acc
    | x :: t -> aux (create_replicates times acc x) (i - 1) t
  in
  aux [] times (List.rev list)

(*Basic solution*)
let fold_replicate list times =
  List.fold_left (create_replicates times) [] (List.rev list)

(* Drop Every N'th Element From a List *)
(* Solution with tail recursion *)
let list_to_drop = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let drop list nth =
  let rec aux acc i = function
    | [] -> acc
    | x :: t -> if i = nth then aux acc 1 t else aux (x :: acc) (i + 1) t
  in
  List.rev (aux [] 1 list)

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
(* Solution *)
let list_to_split = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let split list len =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if i = len then (List.rev acc, l) else aux (h :: acc) (i + 1) t
  in
  aux [] 0 list

(* Extract a Slice From a List *)
(* Solution *)
let list_to_slice = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

let rec drop count point = function
  | [] -> []
  | x :: t -> if count = point then t else drop (count + 1) point t

let slice list i k =
  let first_part = List.rev (drop 1 i list) in
  let second_part = drop 1 (List.length list - k - 1) first_part in
  List.rev second_part

(* Rotate a List N Places to the Left *)
(* Solution *)
let list_to_rotate = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]

let rotate list places =
  let rec aux acc n = function
    | [] -> []
    | x :: t as l ->
        if n = places then l @ List.rev acc else aux (x :: acc) (n + 1) t
  in
  aux [] 0 list

(* Remove the K'th Element From a List *)
(* Solution *)

let list_to_remove_kth = [ "a"; "b"; "c"; "d" ]
