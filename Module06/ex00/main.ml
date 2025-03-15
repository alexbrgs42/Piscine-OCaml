let () =
  let p = new People.people ("Steve") in
  print_endline "\n-------- to_string --------";
  print_endline (p#to_string);
  print_endline "\n-------- talk --------";
  p#talk;
  print_endline "\n-------- die --------";
  p#die
