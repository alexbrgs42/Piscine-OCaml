let ft_print_alphabet () =
  let rec print_letter c =
    print_char c;
    if c <> 'z' then
      print_letter (char_of_int (int_of_char c + 1))
  in print_letter 'a';
  print_char '\n'

let main () =
  ft_print_alphabet ()

let () = main ()