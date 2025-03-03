let ft_string_all f str =
  let rec aux i =
    if i < String.length str then
      f (String.get str i) && aux (i + 1)
    else true
  in aux 0

(* 
let is_digit c = c >= '0' && c <= '9'

let print_result res =
  if res = true then
    print_endline "true"
  else
    print_endline "false"

let main () =
  print_result (ft_string_all is_digit "0123456789");
  print_result (ft_string_all is_digit "O12EAS67B9");
  print_result (ft_string_all is_digit "")

let () = main () *)
