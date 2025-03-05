let print_value_list l =
  print_string "[";
  let rec aux l = match l with
    | [] -> print_string "]\n"
    | head::[] -> print_string (Value.toStringVerbose head);
                  aux []
    | head::tail -> print_string (Value.toStringVerbose head);
                    print_string ", ";
                    aux tail
  in aux l

let print_next card_value =
  let n = Value.toStringVerbose (Value.next card_value) in
  print_string "Value.next for card value ";
  print_string (Value.toStringVerbose card_value);
  print_string " is : ";
  print_endline n

let print_previous card_value =
  let n = Value.toStringVerbose (Value.previous card_value) in
  print_string "Value.previous for card value ";
  print_string (Value.toStringVerbose card_value);
  print_string " is : ";
  print_endline n

let main () =
  print_int (Value.toInt Value.T5);
  print_string "\n";
  print_endline (Value.toString Value.T5);
  print_value_list Value.all;
  try print_next Value.As with e -> print_endline (Printexc.to_string e);
  try print_previous Value.T2 with e -> print_endline (Printexc.to_string e);
  try print_previous Value.As with e -> print_endline (Printexc.to_string e)

let () = main ()
