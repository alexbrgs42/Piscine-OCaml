let crossover l1 l2 =
  if l1 = [] || l2 = [] then
    []
  else
    let rec is_common x l2 = match l2 with
      | [] -> false
      | head::tail when head = x -> true
      | _::tail -> is_common x tail
    in
    let rec create_common_list l1 acc = match l1, acc with
      | (a::tail), [] when is_common a l2 -> create_common_list tail [a]
      | (a::tail), h when is_common a l2 -> create_common_list tail (a::acc)
      | (a::tail), _ -> create_common_list tail acc
      | _ -> acc
    in
    let rec inv_list l = match l with
      | [] -> []
      | head::tail -> (inv_list tail)@[head]
    in create_common_list (inv_list l1) []

let rec print_int_list l = match l with
  | [] -> print_string "[]\n"
  | a::[] -> Printf.printf "%d\n" a
  | a::tail -> Printf.printf "%d; " a; print_int_list tail

let rec print_char_list l = match l with
  | [] -> print_string "[]\n"
  | a::[] -> Printf.printf "%c\n" a
  | a::tail -> Printf.printf "%c; " a; print_char_list tail

let () =
  print_int_list (crossover [1; 2] [4; 7; 2]);
  print_char_list (crossover ['f'; 'h'] ['g']);
  print_char_list (crossover ['f'; 'h'] []);
  print_char_list (crossover ['f'; 'h'; 'u'] ['u'; 'f'; 'h'])
