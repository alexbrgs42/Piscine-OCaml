let split_line str =
  let arr = Array.of_list (String.split_on_char ',' str) in
  let len = Array.length arr in
  let new_arr = Array.make (len - 1) 0. in
  for i = 0 to (len - 2) do
    new_arr.(i) <- float_of_string arr.(i)
  done;
  (new_arr, arr.(len - 1))

let examples_of_file filename =
  let fd = open_in filename in
  let lst = ref [] in
  try
    while (true) do
      lst := !lst@[input_line fd]
    done;
    []
  with End_of_file -> ();
  List.map split_line !lst

let toStringCouple (arr, l) =
  let str = ref "([|" in
  let len = Array.length arr in
  for i = 0 to (len - 1) do
    str := !str^(string_of_float arr.(i));
    if i <> len - 1 then
      str := !str^"; "
  done;
  !str^"|], \""^l^"\")"

let toStringList lst =
  let len = List.length lst in
  let str = ref "[" in
  for i = 0 to (len - 1) do
    str := !str^(toStringCouple (List.nth lst i));
    if i <> len - 1 then
      str := !str^"\n"
  done;
  !str^"]"

let main argc argv = 
  print_endline (toStringList (examples_of_file argv.(1)))

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
