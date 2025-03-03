let print_number nb =
  if nb < 10 then
    print_int 0;
  print_int nb

let ft_print_comb2 () =
  let rec aux first_nb second_nb =
    print_number first_nb;
    print_char ' ';
    print_number second_nb;
    if first_nb = 98 then
      print_char '\n'
    else
      begin
        print_char ',';
        print_char ' ';
        if second_nb < 99 then
          aux first_nb (second_nb + 1)
        else
          aux (first_nb + 1) (first_nb + 2)
      end
  in aux 0 0
(* 
let main () =
  ft_print_comb2 ()

let () = main () *)
