let () =
  let water = new Molecule.water in
  let carbon_dioxide = new Molecule.carbon_dioxide in
  let trinitrotoluene = new Molecule.trinitrotoluene in
  let methane = new Molecule.methane in
  let dioxygen = new Molecule.dioxygen in

  print_endline water#to_string;
  print_endline carbon_dioxide#to_string;
  print_endline trinitrotoluene#to_string;
  print_endline methane#to_string;
  print_endline dioxygen#to_string;

  print_endline ("dioxygen = methane: " ^ (if dioxygen#equals methane then "true" else "false"));
  print_endline ("water = water: " ^ (if water#equals water then "true" else "false"))
