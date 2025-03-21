class virtual molecule name atom_list =
  object (self)
    method get_name : string = name
    method get_formula :string =
      let sorted_atom_list = List.sort self#atoms_order atom_list in
      let sorted_atom_symbol = List.map (fun elem -> elem#get_symbol) sorted_atom_list in
      let rec aux lst symbole i = match lst, i with
        | (head::tail, 0) -> head ^ (aux tail head 1)
        | (head::tail, 1) when head <> symbole -> head ^ (aux tail head 1)
        | (head::tail, _) when head <> symbole -> (string_of_int i) ^ head ^ (aux tail head 1)
        | (_::tail, _) -> aux tail symbole (i + 1)
        | ([], j) when j > 1 -> string_of_int j
        | _ -> ""
      in
      aux sorted_atom_symbol "" 0

    method to_string = "Molecule: " ^ "{ name=" ^ self#get_name ^ ", formula=" ^ self#get_formula ^ " }"
    method equals (that : molecule) =
      that#get_name = self#get_name &&
      that#get_formula = self#get_formula
    method atoms_order (a:Atom.atom) (b:Atom.atom) = match a#get_symbol, b#get_symbol with
      | x, y when x = y -> 0
      | x, _ when x = "C" -> (-1)
      | x, y when x = "H" && y <> "C" -> (-1)
      | _, y when y = "C" -> 1
      | x, y when y = "H" && x <> "C" -> 1
      | x, y -> String.compare x y
  end

class water =
  object
    inherit molecule "water" [new Atom.hydrogen; new Atom.oxygen; new Atom.hydrogen]
  end

class carbon_dioxide =
  object
    inherit molecule "carbon dioxide" [new Atom.carbon; new Atom.oxygen; new Atom.oxygen]
  end

class carbon =
  object
    inherit molecule "carbon" [new Atom.carbon]
  end

class carbon_monoxide =
  object
    inherit molecule "carbon monoxide" [new Atom.carbon; new Atom.oxygen]
  end

class trinitrotoluene =
  object
    inherit molecule "trinitrotoluene" [new Atom.nitrogen; new Atom.nitrogen; new Atom.nitrogen;
    new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
    new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen;
    new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon]
  end

class methane =
  object
    inherit molecule "methane" [new Atom.hydrogen; new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen]
  end

class dioxygen =
  object
    inherit molecule "dioxygen" [new Atom.oxygen; new Atom.oxygen]
  end