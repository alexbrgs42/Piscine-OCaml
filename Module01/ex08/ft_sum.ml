let ft_sum f a b =
  if a > b then
    nan
  else
    let rec aux res i =
      if i = b + 1 then
        res
      else
        aux (res +. f i) (i + 1)
    in aux 0. a

let () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_char '\n';
  print_float (ft_sum (fun i -> float_of_int i) 0 10);
  print_char '\n'
