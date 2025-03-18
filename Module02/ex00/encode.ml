let encode lst =
  let rec aux lst acc = match lst, acc with
    | [], _ ->  acc
    | (x::l), ((n, a)::t) when x = a -> aux l ((n + 1, a)::t)
    | (x::l), _ -> aux l ((1, x)::acc)
  in
  let rec inv l acc = match l with
    | [] -> acc
    | a::t -> inv t ([a]@acc)
  in aux (inv lst []) []

let rec print_char_list l = match l with
  | [] -> print_string "\n"
  | (a, b)::[] -> Printf.printf "(%d, %c)" a b; print_char_list []
  | (a, b)::tail -> Printf.printf "(%d, %c), " a b; print_char_list tail

let rec print_int_list l = match l with
  | [] -> print_string "\n"
  | (a, b)::[] -> Printf.printf "(%d, %d)" a b; print_int_list []
  | (a, b)::tail -> Printf.printf "(%d, %d), " a b; print_int_list tail
    
let () =
  print_char_list (encode ['a'; 'a'; 'a'; 'b'; 'b'; 'b']);
  print_int_list (encode [1; 5; 3; 3; 0]);
  print_endline (if encode [] = [] then "empty list" else "non empty list")
