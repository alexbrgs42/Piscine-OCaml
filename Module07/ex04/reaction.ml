class virtual reaction _start _end =
  object
    method get_start : (Molecule.molecule * int) list = _start
    method get_end : (Molecule.molecule * int) list = _end
    method virtual balance : reaction
    method virtual is_balanced : bool
  end

class alkane_combustion alkanes =
  object (self)
    inherit reaction (List.map (fun molecule -> (molecule, 1)) alkanes) []

    method balance = List.iter (fun x -> Printf.printf "(%s, %d)\n" ((fst x)#to_string) (snd x)) self#get_start; new alkane_combustion []
    method is_balanced = true
  end
