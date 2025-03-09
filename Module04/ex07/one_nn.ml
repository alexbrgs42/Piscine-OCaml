(* PARSE DATA *)

let split_line str =
  let arr = Array.of_list (String.split_on_char ',' str) in
  let len = Array.length arr in
  let new_arr = Array.make (len - 1) 0. in
  for i = 0 to (len - 2) do
    new_arr.(i) <- float_of_string arr.(i)
  done;
  (new_arr, arr.(len - 1))

let examples_of_file filename =
  let fd = open_in filename in
  let lst = ref [] in
  try
    while (true) do
      lst := !lst@[input_line fd]
    done;
    []
  with End_of_file -> ();
  List.map split_line !lst

(* EUCLIDIAN DISTANCE *)

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

(* ONE-nearest neighbors algorithm *)

type radar = float array * string

let one_nn list_radar model_radar =
  let list_vector = fst (List.split list_radar) in
  let list_eu_dist = List.map (eu_dist (fst model_radar)) list_vector in
  let min_eu_dist = List.fold_left min (List.hd list_eu_dist) list_eu_dist in
  let i = ref 0 in
  while (min_eu_dist <> (Array.of_list list_eu_dist).(!i)) do
    incr i
  done;
  snd (Array.of_list list_radar).(!i)

(* Display functions *)

let toStringCouple (arr, l) =
  let str = ref "([|" in
  let len = Array.length arr in
  for i = 0 to (len - 1) do
    str := !str^(string_of_float arr.(i));
    if i <> len - 1 then
      str := !str^"; "
  done;
  !str^"|], \""^l^"\")"

let main argc argv =
  let model1 = ([|1.; 0.; 0.8471; 0.13533; 0.73638; -0.06151; 0.87873; 0.0826; 0.88928; -0.09139; 0.78735; 0.06678; 0.80668; -0.00351; 0.79262; -0.01054; 0.85764; -0.04569; 0.8717; -0.03515; 0.81722; -0.0949; 0.71002; 0.04394; 0.86467; -0.15114; 0.81147; -0.04822; 0.78207; -0.00703; 0.75747; -0.06678; 0.85764; -0.06151|], "g") in
  print_endline ( (one_nn (examples_of_file argv.(1)) model1));
  let model2 = ([|1.; 1.; 0.8471; 0.13533; 0.73638; -0.06151; 0.87873; 0.0826; 0.88928; -0.09139; 0.78735; 0.06678; 0.80668; -0.00351; 0.79262; -0.01054; 0.85764; -0.04569; 0.8717; -0.03515; 0.81722; -0.0949; 0.71002; 0.04394; 0.86467; -0.15114; 0.81147; -0.04822; 0.78207; -0.00703; 0.75747; -0.06678; 0.85764; -0.06151|], "g") in
  print_endline ( (one_nn (examples_of_file argv.(1)) model2))

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
