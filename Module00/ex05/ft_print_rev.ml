let ft_print_rev s =
  let rec aux str i =
    if i > 0 then
      begin
        print_char (String.get str (i - 1));
        aux str (i - 1)
      end
  in aux s (String.length s);
  print_char '\n'

(* 
let main () =
  ft_print_rev "Hello world !";
  ft_print_rev ""

let () = main () *)
