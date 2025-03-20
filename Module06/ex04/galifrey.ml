class galifrey (people : People.people list) (doctors : Doctor.doctor list) (daleks : Dalek.dalek list) =
  object (self)
    val _people = people
    val _doctors = doctors
    val _daleks = daleks
    
    initializer Random.self_init ()
    method get_people = _people
    method get_doctors = _doctors
    method get_daleks = _daleks

    method do_time_war =
      let rec aux (army1 : People.people list) (army2 : Doctor.doctor list) (army3 : Dalek.dalek list) = match army1, army2, army3, Random.int 6 with
        | [], [], _, _ -> print_endline "Daleks won !\n"
        | [], _, [], _ -> print_endline "Doctors won !\n"
        | _, [], [], _ -> print_endline "People won !\n"
        | [], _, _, i when i < 3 -> let l1, l2 = (((List.hd army2)#take_damage (List.hd army3)#attack)::(List.tl army2), army3) in
                      aux [] ((List.hd l1)#check_death l1) ((List.hd l2)#check_death l2)
        | [], _, _, i -> let l1, l2 = (army2, ((List.hd army3)#take_damage (List.hd army2)#attack)::(List.tl army3)) in
                      aux [] ((List.hd l1)#check_death l1) ((List.hd l2)#check_death l2)

        | _, [], _, i when i < 3 -> let l1, l2 = (((List.hd army1)#take_damage (List.hd army3)#attack)::(List.tl army1), army3) in
                      aux ((List.hd l1)#check_death l1) [] ((List.hd l2)#check_death l2)
        | _, [], _, i -> let l1, l2 = (army1, ((List.hd army3)#take_damage (List.hd army1)#attack)::(List.tl army3)) in
                      aux ((List.hd l1)#check_death l1) [] ((List.hd l2)#check_death l2)

        | _, _, [], i when i < 3 -> let l1, l2 = (((List.hd army1)#take_damage (List.hd army2)#attack)::(List.tl army1), army2) in
                      aux ((List.hd l1)#check_death l1) ((List.hd l2)#check_death l2) []
        | _, _, [], i -> let l1, l2 = (army1, ((List.hd army2)#take_damage (List.hd army1)#attack)::(List.tl army2)) in
                      aux ((List.hd l1)#check_death l1) ((List.hd l2)#check_death l2) []
        | _, _, _, i -> let a, b, c = match i with
          | 0 -> (((List.hd army1)#take_damage (List.hd army2)#attack)::(List.tl army1), army2, army3)
          | 1 -> (((List.hd army1)#take_damage (List.hd army3)#attack)::(List.tl army1), army2, army3)
          | 2 -> (army1, ((List.hd army2)#take_damage (List.hd army1)#attack)::(List.tl army2), army3)
          | 3 -> (army1, ((List.hd army2)#take_damage (List.hd army3)#attack)::(List.tl army2), army3)
          | 4 -> (army1, army2, ((List.hd army3)#take_damage (List.hd army1)#attack)::(List.tl army3))
          | _ -> (army1, army2, ((List.hd army3)#take_damage (List.hd army2)#attack)::(List.tl army3))
          in
          aux ((List.hd a)#check_death a) ((List.hd b)#check_death b) ((List.hd c)#check_death c)
      in aux self#get_people self#get_doctors self#get_daleks
  end