open Printf

let read_whole_chan chan =
  let buf = Buffer.create 4096 in
  let rec loop () =
    let line = input_line chan in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    loop ()
  in
  try loop () with End_of_file -> Buffer.contents buf

let read_whole_file filename =
  let chan = open_in filename in
  read_whole_chan chan

let main () =
  let filename = Sys.argv.(1) in
  let str = read_whole_file filename in
  printf "I read %d characters from %s\n" (String.length str) filename

let () = main ()
