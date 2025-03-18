let main () =
  List.iter (fun elem -> print_endline (Color.toString elem)) Color.all;
  List.iter (fun elem -> print_endline (Color.toStringVerbose elem)) Color.all

let () = main ()
