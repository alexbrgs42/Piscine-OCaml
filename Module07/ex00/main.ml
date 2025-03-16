let () =
  let hydrogen = new Atom.hydrogen in
  let carbon = new Atom.carbon in
  let oxygen = new Atom.oxygen in
  let nitrogen = new Atom.nitrogen in
  let helium = new Atom.helium in
  let sulfur = new Atom.sulfur in
  let sulfur_bis = new Atom.sulfur in

  print_endline (hydrogen#to_string);
  print_endline (carbon#to_string);
  print_endline (oxygen#to_string);
  print_endline (nitrogen#to_string);
  print_endline (helium#to_string);
  print_endline (sulfur#to_string);
  print_endline (sulfur_bis#to_string);

  print_endline ("hydrogen = carbon: " ^ (if hydrogen#equals carbon then "true" else "false"));
  print_endline ("sulfur = sulfur: "  ^ (if sulfur#equals sulfur then "true" else "false"));
  print_endline ("sulfur = sulfur_bis: "  ^ (if sulfur#equals sulfur_bis then "true" else "false"))
