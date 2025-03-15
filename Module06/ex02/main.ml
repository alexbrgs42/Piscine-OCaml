let () =
  let human = new People.people ("Dan") in
  let doctor = new Doctor.doctor "Who" 45 human in
  let dalek = new Dalek.dalek in
  
  print_endline dalek#to_string;
  dalek#talk;
  
  print_endline "\n---------------------- ⚔️ BATTLE ⚔️ ----------------------";
  print_endline "\n---------------------- FIRST ROUND ----------------------\n";

  let dalek_round_1 = dalek#take_damage 50 in
  dalek_round_1#exterminate human;
  let doctor_round_1 = doctor#take_damage 10 in
  
  print_endline "\n---------------------- ROUND RESULT ----------------------\n";

  print_endline "human died.";
  print_endline dalek_round_1#to_string;
  print_endline doctor_round_1#to_string;

  print_endline "\n---------------------- SECOND ROUND ----------------------\n";

  let dalek_round_2 = dalek_round_1#take_damage 70 in
  let doctor_round_2 = doctor_round_1#take_damage 85 in

  print_endline "\n---------------------- ROUND RESULT ----------------------\n";

  print_endline dalek_round_2#to_string;
  print_endline doctor_round_2#to_string;

  print_endline "\n---------------------- THIRD ROUND ----------------------\n";

  let doctor_round_3 = doctor_round_2#take_damage 10 in

  print_endline "\n---------------------- ROUND RESULT ----------------------\n";

  print_endline dalek_round_2#to_string;
  print_endline doctor_round_3#to_string;

  print_endline "\n---------------------- FOURTH ROUND ----------------------\n";

  (dalek_round_2#take_damage 40)#die;

  print_endline "\n---------------------- ROUND RESULT ----------------------\n";

  print_endline "THE WINNER IS 🏆 DOCTOR 🏆 !!!!!\n"