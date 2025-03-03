let converges f x n =
  if n < 0 then
    false
  else
    let rec aux res i =
      if res = f res then
        true
      else if i = 0 then
        false
      else
        aux (f res) (i - 1)
    in aux x n

let () =
  let res = converges (( * ) 2) 2 5 in
  if res = true then
    print_string "true"
  else
    print_string "false";
  print_char '\n';
  let res = converges (fun x -> x / 2) 2 3 in
  if res = true then
    print_string "true"
  else
    print_string "false";
  print_char '\n';
  let res = converges (fun x -> x / 2) 2 2 in
  if res = true then
    print_string "true"
  else
    print_string "false";
  print_char '\n'
