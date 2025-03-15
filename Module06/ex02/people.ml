class people name =
  object
    val name:string = name
    val hp:int = 100

    initializer print_endline ("People: object created with attribute " ^ name ^ " !")
    method to_string = "People: My name is " ^ name ^ ", I have " ^ (string_of_int hp) ^ " hp."
    method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"

    method take_damage damage =
      {< name = name; hp = if hp - damage > 0 then hp - damage else 0 >}
  end
