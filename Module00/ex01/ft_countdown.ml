let rec ft_countdown nb =
  if nb <= 0 then
    begin
      print_int 0;
      print_char '\n'
    end
  else
    begin
      print_int nb;
      print_char '\n';
      ft_countdown (nb - 1)
    end

(*
let main () =
  ft_countdown 3;
  print_char '\n';
  ft_countdown 0;
  print_char '\n';
  ft_countdown (-1)

let () = main () *)
