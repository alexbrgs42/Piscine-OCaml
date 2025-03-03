let fibonacci n =
  if n < 0 then
    (-1)
  else if n = 0 then
    0
  else if n = 1 then
    1
  else
    let rec aux i prev1 prev2 =
      if i = n then
        (prev1 + prev2)
      else
        aux (i + 1) (prev1 + prev2) prev1
    in aux 2 1 0

let () =
  print_int (fibonacci (-42));
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n'
