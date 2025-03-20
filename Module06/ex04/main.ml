let () =
  let people0 = new Army.army in
  let people1 = people0#add (new People.people "A") in
  let people2 = people1#add (new People.people "B") in
  let people3 = people2#add (new People.people "C") in
  let people4 = people3#add (new People.people "D") in

  let doctor0 = new Army.army in
  let doctor1 = doctor0#add (new Doctor.doctor "a" 30 (new People.people "aa")) in
  let doctor2 = doctor1#add (new Doctor.doctor "b" 31 (new People.people "bb")) in
  let doctor3 = doctor2#add (new Doctor.doctor "c" 32 (new People.people "cc")) in
  let doctor4 = doctor3#add (new Doctor.doctor "d" 33 (new People.people "dd")) in

  let dalek0 = new Army.army in
  let dalek1 = dalek0#add (new Dalek.dalek) in
  let dalek2 = dalek1#add (new Dalek.dalek) in
  let dalek3 = dalek2#add (new Dalek.dalek) in
  let dalek4 = dalek3#add (new Dalek.dalek) in

  let g = new Galifrey.galifrey people4#get_members doctor4#get_members dalek4#get_members in

  print_endline "";

  print_endline "⚔️  WAR STARTS ⚔️";
  g#do_time_war
