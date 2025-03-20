class dalek =
  object (self)
    val name = "Dalek" ^ (String.map (fun c -> char_of_int (Random.int 25 + 97 - (if Random.bool () then 0 else 32))) "   ")
    val hp = 100
    val mutable shield = true

    initializer Random.self_init ()
    method to_string = "Dalek: I'm " ^ name ^ ". I have " ^ (string_of_int hp) ^ " hp and my shield is " ^ (if shield = true then "on." else "off.")
    method exterminate (people_to_kill : People.people) = shield <- not shield; people_to_kill#die
    method talk =
      let select_string i = match i with
        | 0 -> "Explain! Explain!"
        | 1 -> "Exterminate! Exterminate!"
        | 2 -> "I obey!"
        | _ -> "You are the Doctor! You are the enemy of the Daleks!"
      in
      print_endline (select_string (Random.int 4))
    method die = print_endline "Emergency Temporal Shift!"
    method attack = match Random.int 4 with
      | 0 -> print_string "Dalek inflects 10 damages to "; 10
      | 1 -> print_string "Dalek inflects 40 damages to "; 40
      | _ -> print_string "Dalek inflects 20 damages to "; 20
    method take_damage damage =
      print_endline "Dalek";
      if damage >= hp then
        {< name = name; hp = 0; shield = shield >}
      else
        {< name = name; hp = hp - damage; shield = shield >}
    method check_death (lst : dalek list) =
      if hp = 0 then
        begin
          if List.tl lst = [] then
            print_endline "All Daleks died.";
          List.tl lst
        end
      else
        lst
  end
