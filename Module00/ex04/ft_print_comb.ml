let ft_print_comb () =
  let rec aux a b c =
    print_int a;
    print_int b;
    print_int c;
    if a < 7 then
      begin
        print_string ", ";
        if c >= 9 then
          begin
            if b + 1 >= 8 then
              aux (a + 1) (a + 2) (b + 2)
            else
              aux a (b + 1) (b + 2);
          end
        else
          aux a b (c + 1);
      end
    else
      print_string "\n";
  in aux 0 1 2

let main () =
  ft_print_comb ()

let () = main ()
