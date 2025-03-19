let prev_gray_code prefix str_code rev =
  let list_code = String.split_on_char ' ' str_code in
  let rec prefix_string l acc = match l with
    | [] -> acc
    | head::tail -> (prefix_string tail ((prefix^head)::acc))
  in 
  let rec rev_list l acc = match l, acc with
    | [], _ -> acc
    | head::tail, [] -> rev_list tail [head]
    | head::tail, _ -> rev_list tail (head::acc)
  in
  if rev = true then
    String.concat " " (prefix_string list_code [])
  else
    String.concat " " (rev_list (prefix_string list_code []) [])

let gray n =
  if n <= 0 then
    ""
  else
    let rec aux acc i = match i with
      | j when j = n -> acc
      | _ -> aux ((prev_gray_code "0" acc false)^" "^(prev_gray_code "1" acc true)) (i + 1)
    in aux "0 1" 1

let () =
  print_endline (gray 1);
  print_endline (gray 2);
  print_endline (gray 3);
  print_endline (gray 0);
  print_endline (gray (-1))
