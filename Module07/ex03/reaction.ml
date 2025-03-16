class virtual reaction _start _end =
  object
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_end : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
