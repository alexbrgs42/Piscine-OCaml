class virtual reaction _start _result =
  object (self)
    method get_start : (Molecule.molecule * int) list =
      if self#is_balanced = false then
        failwith "Unbalanced reaction."
      else
        _start
    method get_result : (Molecule.molecule * int) list =
      if self#is_balanced = false then
        failwith "Unbalanced reaction."
      else
        _result
    method virtual balance : reaction
    method virtual is_balanced : bool

  end

let get_n_by_alkane_name alkane = match alkane#get_name with
  | "methane" -> 1
  | "ethane" -> 2
  | "propane" -> 3
  | "butane" -> 4
  | "pentane" -> 5
  | "hexane" -> 6
  | "heptane" -> 7
  | "octane" -> 8
  | "nonane" -> 9
  | "decane" -> 10
  | "undecane" -> 11
  | _ -> 12

let is_alkane molecule = match molecule#get_name with
  | "methane" | "ethane" | "propane" | "butane" | "pentane"
  | "hexane" | "heptane" | "octane" | "nonane" | "decane"
  | "undecane" | "dodecane" -> true
  | _ -> false

let is_dioxygen molecule = match molecule#get_name with
  | "dioxygen" -> true
  | _ -> false

let is_carbon_dioxide molecule = match molecule#get_name with
  | "carbon dioxide" -> true
  | _ -> false

let is_water molecule = match molecule#get_name with
  | "water" -> true
  | _ -> false

let start lst =
  let start = List.filter (fun m -> (is_alkane (fst m)) || (is_dioxygen (fst m))) lst in
  List.sort (fun a b -> compare (fst a)#get_formula (fst b)#get_formula) start

let result lst = 
  let result = List.filter (fun m -> (is_carbon_dioxide (fst m)) || (is_water (fst m))) lst in
  List.sort (fun a b -> compare (fst a)#get_formula (fst b)#get_formula) result

class alkane_combustion alkanes =
  object (self)
    inherit reaction (start alkanes) (result alkanes)

    method balance =
      let start_molecules = self#group_mol (start alkanes) in
      let alkane = (List.hd (List.filter (fun x -> is_alkane (fst x)) start_molecules)) in
      let n = get_n_by_alkane_name (fst alkane) in
      let alkane_cf = (if n mod 2 = 0 then 2 else 1) in
      new alkane_combustion [
        (Oo.copy (fst alkane), alkane_cf);
        (new Molecule.dioxygen, alkane_cf * (3 * n + 1) / 2);
        (new Molecule.carbon_dioxide, alkane_cf * n);
        (new Molecule.water, alkane_cf * (n + 1))]

    method private group_mol (lst : (Molecule.molecule * int) list) =
      let rec aux (l : (Molecule.molecule * int) list) (acc : (Molecule.molecule * int) list) = match l, acc with
        | [], _ -> acc
        | (a, b)::tail1, (x, y)::tail2 when a#get_name = x#get_name -> aux tail1 ((x, y + b)::tail2)
        | (a, b)::tail, _ -> aux tail ((a, b)::acc)
      in aux lst []
  
    method is_balanced =
      if List.length (start alkanes) + List.length (result alkanes) <> List.length alkanes then
        false
      else
        let start_molecules = self#group_mol (start alkanes) in
        let result_molecules = self#group_mol (result alkanes) in
        let alkane = (List.hd (List.filter (fun x -> is_alkane (fst x)) start_molecules)) in
        let alkane_cf = snd alkane in
        let n = get_n_by_alkane_name (fst alkane) in
        if n mod 2 = 0 && alkane_cf mod 2 = 1 then
          false
        else if (List.length start_molecules <> 2) || (List.length result_molecules <> 2) then
          false
        else
          let rec balance_start lst = match lst with
            | (a, b)::tail when is_dioxygen a -> b = alkane_cf * (3 * n + 1) / 2
            | _::tail -> balance_start tail
            | _ -> true
          in
          let rec balance_result lst = match lst with
            | (a, b)::tail when is_carbon_dioxide a -> (b = n * alkane_cf) && (balance_result tail)
            | (_, b)::tail -> (b = (n + 1) * alkane_cf) && (balance_result tail)
            | _ -> true
          in
          (balance_start start_molecules) && (balance_result result_molecules)

  end
