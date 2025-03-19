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

(* K-nearest neighbors algorithm *)

type radar = float array * string

let k_nn lst k model_radar =
  if lst = [] then
    ""
  else
    let dist_radar = List.map (fun x -> (eu_dist (fst x) (fst model_radar), snd x)) lst in
    let ordered_dist_radar = List.sort (fun a b -> compare (fst a) (fst b)) dist_radar in
    let len = List.length ordered_dist_radar in
    let i = ref 0 in
    let res = ref "" in
    while (!i < k && !i < len) do
      res := !res ^ (snd ((Array.of_list ordered_dist_radar).(!i)));
      if (!i < k - 1 && !i < len - 1) then
        res := !res ^ ", ";
      incr i
    done;
    !res

(* main *)

let main argc argv =
  let model1 = ([|1.; 0.; 0.8471; 0.13533; 0.73638; -0.06151; 0.87873; 0.0826; 0.88928; -0.09139; 0.78735; 0.06678; 0.80668; -0.00351; 0.79262; -0.01054; 0.85764; -0.04569; 0.8717; -0.03515; 0.81722; -0.0949; 0.71002; 0.04394; 0.86467; -0.15114; 0.81147; -0.04822; 0.78207; -0.00703; 0.75747; -0.06678; 0.85764; -0.06151|], "g") in
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model1));
  let model2 = ([|1.; 0.; 0.50932; -0.93996; 1.; 0.26708; -0.03520; -1.; 1.; -1.; 0.43685; -1.; 0.; 0.; -1.; -0.34265; -0.37681; 0.03623; 1.; -1.; 0.; 0.; 0.; 0.; -0.16253; 0.92236; 0.39752; 0.26501; 0.; 0.; 1.; 0.23188; 0.; 0.|], "b") in
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model2))

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
