class doctor name age sidekick =
  object (self)
    val name:string = name
    val age:int = age
    val sidekick : (People.people) = sidekick
    val hp:int = 100

    initializer print_endline ("Doctor: object created with attributes " ^ name ^
      ", " ^ (string_of_int age) ^ " and {" ^ sidekick#to_string ^ "} as the sidekick !")

    method to_string = "Doctor: I'm " ^ name ^ ", " ^ (string_of_int age) ^
    " years old. I have " ^ (string_of_int hp) ^ " hp and {" ^ sidekick#to_string ^ "} as the sidekick !"
    method talk = print_endline "Hi! I'm the Doctor!"
    method travel_in_time start arrival =
      let new_age = arrival - start + age in
      if new_age < 0 then
          print_endline "I cannot use the TARDIS, I wasn't even born back then !!";
      print_endline "
      ###########
      ##### #####
      ##  # #  ##
      ##  # #  ##
      ##### #####
      #         #
      #   #     #
      #     #   #
      #         #
      ###########\n";
      {< name = name; age = (if new_age < 0 then age else new_age); sidekick = sidekick; hp = hp >}
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method attack = match Random.int 4 with
      | 0 -> print_string "Doctor inflects 20 damages to "; 20
      | 1 -> print_string "Doctor inflects 30 damages to "; 30
      | 2 -> print_string "Doctor inflects 5 damages to "; 5
      | _ -> print_string "Doctor inflects 50 damages to "; 50
    method take_damage damage =
      print_endline "Doctor";
      if damage >= hp then
        {< name = name; age = age; sidekick = sidekick; hp = 0 >}
      else
        {< name = name; age = age; sidekick = sidekick; hp = hp - damage >}
    method check_death (lst : doctor list) =
      if hp = 0 then
        begin
          if List.tl lst = [] then
            print_endline "All Doctors died.";
          List.tl lst
        end
      else
        lst

    method private regenerate = {< name = name; age = age; sidekick = sidekick; hp = 100 >}
  end
