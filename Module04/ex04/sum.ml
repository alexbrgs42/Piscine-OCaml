let sum a b = a +. b

let main () =
  let a = ref 0.0 in
  let b = ref 2.33 in
  print_string ("sum "^(string_of_float !a)^" "^(string_of_float !b)^" = ");
  print_endline (string_of_float (sum !a !b));

  a := nan;
  print_string ("sum "^(string_of_float !a)^" "^(string_of_float !b)^" = ");
  print_endline (string_of_float (sum !a !b));

  a := (-65.);
  print_string ("sum "^(string_of_float !a)^" "^(string_of_float !b)^" = ");
  print_endline (string_of_float (sum !a !b))

let () = main ()
