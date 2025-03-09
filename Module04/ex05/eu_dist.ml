let eu_dist a b =
  let len_a = Array.length a in
  let len_b = Array.length b in
  let res = ref 0. in
  if len_a = len_b then
    begin
      for i = 0 to (len_a - 1) do
        res := !res +. (a.(i) -. b.(i))**2.
      done;
      sqrt !res
    end
  else
    nan

let main () =
  let a = [|1.43; 4.1|] in
  let b = [|10.; 2.2|] in
  print_endline (string_of_float (eu_dist a b));

  let c = [|3.; 3.; 3.; 3.; 3.; 3.; 3.; 3.; 3.; 3.|] in
  let d = [|4.5; 4.5; 4.5; 4.5; 4.5; 4.5; 4.5; 4.5; 4.5; 4.5|] in
  print_endline (string_of_float (eu_dist c d));
  print_endline (string_of_float (eu_dist a d))

let () = main ()

(* √((1.43−10)²+(4.1−2.2)²) *)
(* √((3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²+(3−4,5)²) *)