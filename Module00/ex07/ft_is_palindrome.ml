let ft_is_palindrome str =
  let rec aux i len =
    if i < len / 2 then
      begin
        if String.get str i = String.get str (len - i - 1) then
          aux (i + 1) len
        else
          false
      end
    else
      true
  in aux 0 (String.length str)
(* 
let print_result res =
  if res = true then
    print_endline "true"
  else
    print_endline "false"

let main () =
  print_result (ft_is_palindrome "radar");
  print_result (ft_is_palindrome "maddam");
  print_result (ft_is_palindrome "car");
  print_result (ft_is_palindrome "")

let () = main () *)
