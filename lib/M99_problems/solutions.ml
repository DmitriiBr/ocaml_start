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

(* Solution with checking length of a list*)
let remove_at_1 k list =
  let len = List.length list in
  if len = 0 then list
  else
    let i = if k > len - 1 then k mod len else k in
    let rec aux acc i = function
      | [] -> []
      | h :: t -> if i = 0 then acc @ t else aux (h :: acc) (i - 1) t
    in
    aux [] i list

(* Solution without checking and without tail recursion *)
let rec remove_at k = function
  | [] -> []
  | h :: t -> if k = 0 then t else h :: remove_at (k - 1) t

(* Insert an Element at a Given Position Into a List *)
(* Solution *)

let list_to_insert = [ "a"; "b"; "c"; "d" ]

let rec insert_at str n = function
  | [] -> [ str ]
  | h :: t as l -> if n = 0 then str :: l else h :: insert_at str (n - 1) t

(* Create a List Containing All Integers Within a Given Range *)
(* Solution *)
let range first last =
  let rec aux first last =
    if first > last then [] else last :: aux first (last - 1)
  in
  if first > last then aux last first else List.rev (aux first last)

(* Extract a Given Number of Randomly Selected Elements From a List *)
(* Solution *)
let list_to_rnd_select = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ]

let rand_select list times =
  Random.init 0;
  let rec extract acc index = function
    | [] -> raise Not_found
    | h :: t ->
        if index = 0 then (h, acc @ t) else extract (h :: acc) (index - 1) t
  in
  let rand_extract len list = extract [] (Random.int len) list in
  let rec aux acc n list len =
    if n = 0 then acc
    else
      let picked, rest = rand_extract len list in
      aux (picked :: acc) (n - 1) rest (len - 1)
  in
  let len = List.length list in
  aux [] (min times len) list len

(* Lotto: Draw N Different Random Numbers From the Set 1..M *)
(* Solution without other functions *)
let lotto_select count num =
  Random.init 0;
  let rec aux acc count n =
    if count = 0 then acc
    else
      let drawn = Random.int n in
      aux (drawn :: acc) (count - 1) (n - 1)
  in
  aux [] count num

(* Solution with random select *)
let lotto_select_2 n m = rand_select (range 1 m) n

(* Generate a Random Permutation of the Elements of a List *)
(* Solution *)
let list_to_permutate = [ "a"; "b"; "c"; "d"; "e"; "f" ]

(* Solution 1, my solution, with splittin on 2 functions*)
let pick_random_from list =
  Random.init 0;
  let rec draw acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else draw (h :: acc) (n - 1) t
  in
  let len = List.length list in
  draw [] (Random.int len) list

let permutation list =
  let rec aux acc n list =
    if n = 0 then acc
    else
      let picked, rest = pick_random_from list in
      aux (picked :: acc) (n - 1) rest
  in
  aux [] (List.length list) list

(* Solution 2, while using already defined function*)
let permutation_2 list = rand_select list (List.length list)
