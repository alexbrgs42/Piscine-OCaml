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

let get_1 (a, _, _) = a
let get_2 (_, a, _) = a

let find_nearest lst =
  let ordered_lst = List.sort (fun (a, b, c) (x, y, z) -> compare x a) lst in
  let rec find_candidate l occurence acc = match l with
    | (a, b, c)::tail when a = occurence -> find_candidate tail occurence ((a, b, c)::acc)
    | _::tail -> find_candidate tail occurence acc
    | [] -> acc
  in
  let tie_lst = find_candidate ordered_lst (get_1 (List.hd ordered_lst)) [] in
  if List.length tie_lst = 1 then
    get_2 (List.hd tie_lst)
  else
    let best_of_tie = List.sort (fun (a, b, c) (x, y, z) -> compare c z) tie_lst in
    get_2 (List.hd best_of_tie)

let k_nn lst k model_radar =
  if lst = [] then
    ""
  else
    let dist_radar = Array.map (fun x -> (eu_dist (fst x) (fst model_radar), snd x)) (Array.of_list lst) in
    Array.sort (fun a b -> compare (fst a) (fst b)) dist_radar;
    let len = Array.length dist_radar in
    let k_nearest = Array.sub dist_radar 0 (min k len) in
    Array.sort (fun a b -> compare (snd a) (snd b)) k_nearest;
    let rec aux lst acc = match lst, acc with
      | [], _ -> find_nearest acc
      | (a, b)::tail, (x, y, coef)::t when b = y -> aux tail ((x + 1, y, coef +. a)::t)
      | (a, b)::tail, _ -> aux tail ((1, b, a)::acc)
    in
    aux (Array.to_list k_nearest) []

let test_accuracy f lst models =
  if lst = [] || models = [||] then
    print_endline ""
  else
    let len_lst = List.length lst in
    let len_models = Array.length models in
    let total = len_lst * len_models in
    let my_res = ref 0. in
    for j = 0 to (len_models - 1) do
      for i = 1 to len_lst do
        if (f lst i (models.(j))) = (f lst len_lst (models.(j))) then my_res := !my_res +. 1.
      done
    done;
    Printf.printf "Based on %d models, the accuracy is : %.4f%% (%d/%d)\n" len_models (!my_res /. (float_of_int total) *. 100.) (int_of_float !my_res) total

(* main *)

let main argc argv =
  let model1 = ([|1.; 0.; 0.8471; 0.13533; 0.73638; -0.06151; 0.87873; 0.0826; 0.88928; -0.09139; 0.78735; 0.06678; 0.80668; -0.00351; 0.79262; -0.01054; 0.85764; -0.04569; 0.8717; -0.03515; 0.81722; -0.0949; 0.71002; 0.04394; 0.86467; -0.15114; 0.81147; -0.04822; 0.78207; -0.00703; 0.75747; -0.06678; 0.85764; -0.06151|], "g") in
  let model2 = ([|1.; 0.; 0.50932; -0.93996; 1.; 0.26708; -0.03520; -1.; 1.; -1.; 0.43685; -1.; 0.; 0.; -1.; -0.34265; -0.37681; 0.03623; 1.; -1.; 0.; 0.; 0.; 0.; -0.16253; 0.92236; 0.39752; 0.26501; 0.; 0.; 1.; 0.23188; 0.; 0.|], "b") in
  let model3 = ([|1.; 0.; 0.91241; 0.04347; 0.94191; 0.02280; 0.94705; 0.05345; 0.93582; 0.01321; 0.91911; 0.06348; 0.92766; 0.12067; 0.92048; 0.06211; 0.88899; 0.12722; 0.83744; 0.14439; 0.80983; 0.11849; 0.77041; 0.14222; 0.75755; 0.11299; 0.73550; 0.13282; 0.66387; 0.15300; 0.70925; 0.10754; 0.65258; 0.11447|], "g") in
  let model4 = ([|1.; 0.; 0.50932; -0.5; 1.; 0.5; -0.5; -1.5; 1.1; -1.4; 0.5; -1.5; 5.; 5.; -1.5; -0.5; -0.5; 0.5; 1.; -1.; 0.; 0.; 0.; 0.; -0.5; 0.45; 0.3; 0.55; 0.; 0.6; 1.5; 0.3; 0.; 0.3|], "b") in
  let model5 = ([|1.; 0.; 0.50932; -0.56; 1.; 0.67; -0.7; -1.01; 1.2; -1.5; 0.8; -1.; 0.; 0.; -1.; -0.34265; -0.5; 0.456; 1.; -1.4; 6.; 0.; 0.; 0.; -0.87; 0.24; 0.7; 0.4; 0.; 0.; 1.; 0.7; 0.; 0.5|], "b") in
  let model6 = ([|10.; 0.; 0.50932; -0.93996; 1.; 0.26708; -0.76; -1.; 1.; -1.; 0.43685; -1.; 0.; 0.; -1.; -0.34265; -0.37681; 0.03623; 1.; -1.; 0.; 0.; 0.; 0.; -0.16253; 0.92236; 0.39752; 0.26501; 0.; 0.; 1.; 0.23188; 0.; 0.|], "b") in
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model1));
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model2));
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model3));
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model4));
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model5));
  print_endline ((k_nn (examples_of_file argv.(1)) 5 model6));
  test_accuracy k_nn (examples_of_file argv.(1)) [|model1; model2; model3; model4; model5; model6|]

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> Printf.printf "%s\n" (Printexc.to_string e)
