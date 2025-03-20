let () =
  let a = new Reaction.alkane_combustion [new Molecule.water] in
  ignore(a#balance)