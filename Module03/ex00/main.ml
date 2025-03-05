let print_color_list t_list = 
  print_string "[";
  let rec aux l = match l with
    | [] -> print_string "]\n"
    | head::[] -> print_string (Color.toStringVerbose head);
                  aux []
    | head::tail -> print_string (Color.toStringVerbose head);
                    print_string ", ";
                    aux tail
  in aux t_list

let main () =
  let a = Color.Spade in
  print_endline (Color.toString a);
  print_endline (Color.toStringVerbose a);
  print_color_list Color.all

let () = main ()
