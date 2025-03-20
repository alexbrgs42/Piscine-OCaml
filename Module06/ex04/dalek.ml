class dalek =
  object
    val name = Random.self_init (); "Dalek" ^ (String.map (fun c -> char_of_int (Random.int 25 + 97 - (if Random.bool () then 0 else 32))) "   ")
    val hp = 100
    val mutable shield = true

    method to_string = "Dalek: I'm " ^ name ^ ". I have " ^ (string_of_int hp) ^ " hp and my shield is " ^ (if shield = true then "on." else "off.")
    method exterminate (people_to_kill:People.people) = shield <- not shield; people_to_kill#die
    method talk =
      let select_string i = match i with
        | 0 -> "Explain! Explain!"
        | 1 -> "Exterminate! Exterminate!"
        | 2 -> "I obey!"
        | _ -> "You are the Doctor! You are the enemy of the Daleks!"
      in
      print_endline (select_string (Random.int 4))
    method die = print_endline "Emergency Temporal Shift!"
  end
