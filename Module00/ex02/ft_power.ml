let ft_power a b =
  let rec pow c =
    if c = 0 then
      1
    else a * pow (c - 1)
  in pow b

(* 
let main () =
  print_int (ft_power 2 4);
  print_char '\n';
  print_int (ft_power 3 0);
  print_char '\n';
  print_int (ft_power 0 5);
  print_char '\n'

let () = main () *)
