let leibniz_pi delta =
  if delta < 0. then
    (-1)
  else
    let rec aux sum i =
      let my_delta = 4. *. atan 1. -. 4. *. sum in
      if my_delta > 0. && my_delta <= delta then
        i
      else if my_delta < 0. && (-1. *. my_delta) <= delta then
        i
      else if i mod 2 = 0 then
        aux (sum +. (1. /. float_of_int (2 * i + 1))) (i + 1)
      else
        aux (sum +. ((-1.) /. float_of_int (2 * i + 1))) (i + 1)
    in aux 1. 1

let () =
  print_int (leibniz_pi (0.01));
  print_char '\n'
