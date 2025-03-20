class virtual reaction _start _end =
  object
    method get_start : (Molecule.molecule * int) list = _start
    method get_end : (Molecule.molecule * int) list = _end
    method virtual balance : reaction
    method virtual is_balanced : bool
  end
