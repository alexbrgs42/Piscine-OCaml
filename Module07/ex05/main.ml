let print_list lst =
  List.iter (fun x -> Printf.printf "(%s, %d); " (fst x)#get_formula (snd x)) lst;
  print_endline ""

let to_string_reaction reaction =
  let rec aux lst acc = match lst with
    | (a, b)::[] -> acc ^ (if b > 1 then (string_of_int b) else "") ^ a#get_formula
    | (a, b)::tail -> aux tail (acc ^ (if b > 1 then (string_of_int b) else "") ^ a#get_formula ^ " + ")
    | [] -> acc
  in
  (aux reaction#get_start "") ^ " -> " ^ (aux reaction#get_result "")

let to_string_imcomplete_result result =
  let rec aux lst acc = match lst with
    | (a, b)::[] -> acc ^ (if b > 1 then (string_of_int b) else "") ^ a#get_formula
    | (a, b)::tail -> aux tail (acc ^ (if b > 1 then (string_of_int b) else "") ^ a#get_formula ^ " + ")
    | [] -> acc
  in
  aux result ""

let () =
  let methane_combustion = [(new Alkane.methane, 1); (new Molecule.trinitrotoluene, 1); (new Molecule.dioxygen, 2); (new Molecule.carbon_dioxide, 1); (new Molecule.water, 2)] in
  let ethane_combustion = [(new Alkane.ethane, 1); (new Alkane.ethane, 1); (new Molecule.dioxygen, 7); (new Molecule.carbon_dioxide, 4); (new Molecule.water, 6)] in
  let propane_combustion = [(new Alkane.propane, 1); (new Molecule.dioxygen, 5); (new Molecule.carbon_dioxide, 3); (new Molecule.water, 4)] in
  let meth = new Reaction.alkane_combustion methane_combustion in
  let eth = new Reaction.alkane_combustion ethane_combustion in
  let prop = new Reaction.alkane_combustion propane_combustion in

  Printf.printf "Methane :\n";
  Printf.printf "is_balanced : %B\n" meth#is_balanced;
  if meth#is_balanced = false then
    try
      print_list meth#get_start;
    with e -> print_endline (Printexc.to_string e);
    try
      print_list meth#get_result;
    with e -> print_endline (Printexc.to_string e);
  else
    Printf.printf "balancing...\n";
    let meth2 =  meth#balance in
    Printf.printf "Start :";
    print_list meth2#get_start;
    Printf.printf "Result :";
    print_list meth2#get_result;
    print_endline (to_string_reaction meth2);

    Printf.printf "\nEthane :\n";
    Printf.printf "is_balanced : %B\n" eth#is_balanced;
    Printf.printf "Start :";
    print_list eth#get_start;
    Printf.printf "Result :";
    print_list eth#get_result;
    let balanced_eth = eth#balance in

    print_endline "\nIncomplete results for ethane :";
    let lst = eth#get_incomplete_results in
    let rec aux (l : (int * (Molecule.molecule * int) list) list) = match l with
      | h::t -> Printf.printf "%s + %dO2 -> %s\n" "C2H6" (fst h) (to_string_imcomplete_result (snd h)); aux t
      | _ -> print_endline ""
    in aux lst;

    print_endline "Incomplete results for propane :";
    let lst = prop#get_incomplete_results in
    let rec aux (l : (int * (Molecule.molecule * int) list) list) = match l with
      | h::t -> Printf.printf "%s + %dO2 -> %s\n" "C3H8" (fst h) (to_string_imcomplete_result (snd h)); aux t
      | _ -> print_endline ""
    in aux lst;

  print_endline "Ethane reaction:";
  print_endline (to_string_reaction eth);
  print_endline "Balanced ethane reaction:";
  print_endline (to_string_reaction balanced_eth)
