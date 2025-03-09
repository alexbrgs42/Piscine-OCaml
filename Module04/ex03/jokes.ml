let main argc argv =
  if argc = 2 then
    begin
      let array_jokes = ref [||] in
      let fd = open_in argv.(1) in
      try
        while (true) do
          array_jokes := Array.append !array_jokes [|input_line fd|]
        done
      with End_of_file -> close_in fd;
      Random.self_init ();
      print_endline (!array_jokes.(Random.int (Array.length !array_jokes)))
    end
  else
    print_endline "Wrong number of arguments."

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> print_endline "An error occured."
