let increase_char n c =
  if c >= 'a' && c <= 'z' then
      char_of_int (((int_of_char c - 97 + n) mod 26) + 97)
  else
    begin
      if c >= 'A' && c <= 'Z' then
        char_of_int (((int_of_char c - 65 + n) mod 26) + 65)
      else
        c
    end

let ft_rot_n n str =
  if n > 0 then
    String.map (increase_char n) str
  else
    str
(* 
let main () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !")

let ()  = main () *)
