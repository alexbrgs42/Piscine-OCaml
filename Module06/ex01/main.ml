let () =
  let p_bob = new People.people ("Bob") in
  let d_dylan = new Doctor.doctor ("Dylan") (29) (p_bob) in

  print_endline "\n------- Doctor Dylan to_string -------";
  print_endline d_dylan#to_string;

  print_endline "\n------- Doctor Dylan talk -------";
  d_dylan#talk;

  print_endline "\n------- Doctor Dylan travel_in_time 1910 1900 -------";
  let d_traveled_dylan = d_dylan#travel_in_time 1910 1900 in

  print_endline "\n------- Doctor Dylan travel_in_time 2000 1900 -------";
  ignore (d_dylan#travel_in_time 2000 1900);

  print_endline "\n------- TRAVELED Doctor Dylan to_string -------";
  print_endline d_traveled_dylan#to_string;

  print_endline "\n------- TRAVELED Doctor Dylan use_sonic_screwdriver -------";
  d_traveled_dylan#use_sonic_screwdriver;

  print_endline "\n------- Doctor Dylan test_damage -------";
  let d_damaged_dylan = d_dylan#test_damage 50 in

  print_endline "\n------- DAMAGED Doctor Dylan to_string -------";
  print_endline d_damaged_dylan#to_string;

  print_endline "\n------- DAMAGED Doctor Dylan test_damage -------";
  print_endline (d_damaged_dylan#test_damage 50)#to_string
