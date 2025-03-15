class doctor name age sidekick =
  object (self)
    val name:string = name
    val age:int = age
    val sidekick:(People.people) = sidekick
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
    
    method take_damage damage =
      if hp - damage <= 0 then
        self#regenerate
      else
        {< name = name; age = age; sidekick = sidekick; hp = hp - damage >}

    method private regenerate = {< name = name; age = age; sidekick = sidekick; hp = 100 >}
  end
