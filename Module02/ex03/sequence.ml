let sequence n =
  if n <= 0 then
    ""
  else
    let rec count_next lst acc = match lst, acc with
      | [], _ -> acc
      | head::tail, [] -> count_next tail ["1"; head]
      | head::tail, (a::b::t) when head = b -> count_next tail ((string_of_int (int_of_string a + 1))::b::t)
      | head::tail, _ -> count_next tail ("1"::head::acc)
    in
    let rec rev_list lst acc = match lst with
      | a::b::tail -> rev_list tail (a::b::acc)
      | _ -> acc
    in
    let rec tuple_list_to_string tuple_lst = match tuple_lst with
      | a::b::tail -> a^b^(tuple_list_to_string tail)
      | a::[] -> a
      | _ -> ""
    in
    let rec loop lst i = match i with
      | j when j = n -> lst
      | _ -> loop (rev_list (count_next lst []) []) (i + 1)
    in tuple_list_to_string (loop ["1"] 1)

let () =
  print_endline (sequence (-1));
  print_endline (sequence 0);
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7)
