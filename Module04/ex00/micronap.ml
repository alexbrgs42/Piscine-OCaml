let my_sleep () = Unix.sleep 1

let main argc argv =
  if argc = 2 then
    begin
      let n = int_of_string argv.(1) in
      for i = 1 to n do
        my_sleep ()
      done
    end

let () =
  try
    let argv = Sys.argv in
    main (Array.length argv) argv
  with e -> ()

(* ocamlopt unix.cmxa micronap.ml *)
(* .cmxa is is the extension of native code libraries,
  while .cma is the extension of bytecode libraries *)
