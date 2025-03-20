class people name =
  object
    val name:string = name
    val hp:int = 100

    initializer print_endline ("People: object created with attribute " ^ name ^ " !")
    method get_hp = hp
    method to_string = "People: My name is " ^ name ^ ", I have " ^ (string_of_int hp) ^ " hp."
    method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"
    method attack = match Random.int 4 with
      | 0 -> print_string "People inflects 5 damages to "; 5
      | 1 -> print_string "People inflects 5 damages to "; 15
      | 2 -> print_string "People inflects 20 damages to "; 20
      | _ -> print_string "People inflects 35 damages to "; 35
    method take_damage damage =
      print_endline "People";
      if damage >= hp then
        {< name = name; hp = 0 >}
      else
        {< name = name; hp = hp - damage >}
    method check_death (lst : people list) =
      if hp = 0 then
        begin
          if List.tl lst = [] then
            print_endline "All People died.";
          List.tl lst
        end
      else
        lst
  end
