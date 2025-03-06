let print_color_list t_list = 
  Printf.printf "[";
  let rec aux l = match l with
    | [] -> Printf.printf "]\n"
    | head::[] -> Printf.printf "%s" (Color.toStringVerbose head); aux []
    | head::tail -> Printf.printf "%s, " (Color.toStringVerbose head); aux tail
  in aux t_list

let main () =
  let a = Color.Spade in
  Printf.printf "%s\n" (Color.toString a);
  Printf.printf "%s\n" (Color.toStringVerbose a);
  print_color_list Color.all

let () = main ()
