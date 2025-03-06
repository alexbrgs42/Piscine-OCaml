let print_value_list l =
  Printf.printf "[";
  let rec aux l = match l with
    | [] -> Printf.printf "]\n"
    | head::[] -> Printf.printf "%s" (Value.toStringVerbose head);
                  aux []
    | head::tail -> Printf.printf "%s, " (Value.toStringVerbose head); aux tail
  in aux l

let print_next card_value =
  let n = Value.toStringVerbose (Value.next card_value) in
  Printf.printf "Value.next for card value %s is : %s\n" (Value.toStringVerbose card_value) n

let print_previous card_value =
  let n = Value.toStringVerbose (Value.previous card_value) in
  Printf.printf "Value.previous for card value %s is : %s\n" (Value.toStringVerbose card_value) n

let main () =
  Printf.printf "%d\n" (Value.toInt Value.T5)
  Printf.printf "%s\n" (Value.toString Value.T5);
  print_value_list Value.all;
  try print_next Value.As with e -> Printf.printf "%s\n" (Printexc.to_string e);
  try print_previous Value.T2 with e -> Printf.printf "%s\n" (Printexc.to_string e);
  try print_previous Value.As with e -> Printf.printf "%s\n" (Printexc.to_string e)

let () = main ()
