let () =
  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in
  let propane = new Alkane.propane in
  let butane = new Alkane.butane in
  let pentane = new Alkane.pentane in
  let hexane = new Alkane.hexane in
  let heptane = new Alkane.heptane in
  let octane = new Alkane.octane in
  let nonane = new Alkane.nonane in
  let decane = new Alkane.decane in
  let undecane = new Alkane.undecane in
  let dodecane = new Alkane.dodecane in

  print_endline methane#to_string;
  print_endline ethane#to_string;
  print_endline propane#to_string;
  print_endline butane#to_string;
  print_endline pentane#to_string;
  print_endline hexane#to_string;
  print_endline heptane#to_string;
  print_endline octane#to_string;
  print_endline nonane#to_string;
  print_endline decane#to_string;
  print_endline undecane#to_string;
  print_endline dodecane#to_string