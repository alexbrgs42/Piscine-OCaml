let main () =
  let jokes = [|
    "Why was six nervous? Because seven eight nine.";
    "How do trains eat? They choo-choo.";
    "What's worse than finding a worm in your apple? Half a worm.";
    "What do you call a fly with no wings? A walk.";
    "What do you call a pelican that doesn't fly ? A peli-can't."|] in
  Random.self_init ();
  print_endline (jokes.(Random.int 5))

let () = main ()
