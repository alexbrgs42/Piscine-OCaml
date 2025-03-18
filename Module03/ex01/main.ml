let print_next card_value =
  let n = Value.toStringVerbose (Value.next card_value) in
  Printf.printf "Value.next for card value %s is : %s\n" (Value.toStringVerbose card_value) n

let print_previous card_value =
  let n = Value.toStringVerbose (Value.previous card_value) in
  Printf.printf "Value.previous for card value %s is : %s\n" (Value.toStringVerbose card_value) n

let main () =
  print_endline ("Value.toInt Value.T5 -> "^(string_of_int (Value.toInt Value.T5)));
  print_endline ("Value.toInt Value.As -> "^(string_of_int (Value.toInt Value.As)));
  print_endline "";
  print_endline ("Value.toString Value.T5 -> "^(Value.toString Value.T5));
  print_endline "";
  List.iter (fun elem -> print_string ((Value.toStringVerbose elem)^(if elem = Value.As then "." else ", "))) Value.all;
  print_endline "\n";
  try print_next Value.As with e -> print_endline (Printexc.to_string e);
  try print_previous Value.T2 with e -> print_endline (Printexc.to_string e);
  try print_previous Value.As with e -> print_endline (Printexc.to_string e)

let () = main ()
