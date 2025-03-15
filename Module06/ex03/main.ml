 () =
  let a0 = new Army.army in
  let a1 = a0#add (new People.people "A") in
  let a2 = a1#add (new People.people "B") in
  let a3 = a2#add (new People.people "C") in
  let a4 = a3#add (new People.people "D") in
  let a5 = a4#add (new People.people "F") in

  print_endline ("\nPeople army (size=" ^ (string_of_int (List.length a5#get_members)) ^ "):");
  List.iter (fun member -> print_endline member#to_string) a5#get_members;
  print_endline "End of People army.\n";

  print_endline ("People army after a delete (size=" ^ (string_of_int (List.length a5#delete#get_members)) ^ "):");
  List.iter (fun member -> print_endline member#to_string) (a5#delete)#get_members;
  print_endline "End of People army.\n";

  print_endline "People army after destruction :";
  print_endline ("List size = " ^ (string_of_int (List.length (a5#delete#delete#delete#delete#delete)#get_members)));
  List.iter (fun member -> print_endline member#to_string) (a5#delete#delete#delete#delete#delete)#get_members;
  print_endline "End of People army.\n";  

  let b0 = new Army.army in
  let b1 = b0#add (new Dalek.dalek) in
  let b2 = b1#add (new Dalek.dalek) in
  let b3 = b2#add (new Dalek.dalek) in
  let b4 = b3#add (new Dalek.dalek) in
  let b5 = b4#add (new Dalek.dalek) in

  print_endline ("Dalek army (size=" ^ (string_of_int (List.length b5#get_members)) ^ "):");
  List.iter (fun member -> print_endline member#to_string) b5#get_members;
  print_endline "End of Dalek army.\n";

  let b6 = b5#delete in
  let b7 = b6#delete in
  let b8 = b7#delete in
  let b9 = b8#delete in
  let b10 = b9#delete in
  let b11 = b10#delete in

  print_endline "Dalek army after destruction :";
  print_endline ("List size = " ^ (string_of_int (List.length b11#get_members)));
  List.iter (fun member -> print_endline member#to_string) b11#get_members;
  print_endline "End of Dalek army.\n";

  let c0 = new Army.army in
  let c1 = c0#add (new Doctor.doctor "A" 1 (new People.people "a")) in
  let c2 = c1#add (new Doctor.doctor "B" 2 (new People.people "b")) in
  let c3 = c2#add (new Doctor.doctor "C" 3 (new People.people "c")) in
  let c4 = c3#add (new Doctor.doctor "D" 4 (new People.people "d")) in
  let c5 = c4#add (new Doctor.doctor "E" 5 (new People.people "e")) in

  print_endline ("Doctor army (size=" ^ (string_of_int (List.length c5#get_members)) ^ "):");
  List.iter (fun member -> print_endline member#to_string) c5#get_members;
  print_endline "End of Doctor army.\n";

  print_endline "Doctor army after destruction :";
  print_endline ("List size = " ^ (string_of_int (List.length (c5#delete#delete#delete#delete#delete)#get_members)));
  List.iter (fun member -> print_endline member#to_string) (c5#delete#delete#delete#delete#delete)#get_members;
  print_endline "End of Doctor army.\n";  
