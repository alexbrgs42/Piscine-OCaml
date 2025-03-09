type 'a ft_ref = {mutable contents : 'a}

let return x = {contents = x}

let get x_ref = x_ref.contents

let set x_ref y = x_ref.contents <- y

let bind x_ref f : 'b ft_ref = 
  f (get x_ref)

let () =
  let a = return 1 in
  print_int (get a);
  print_char '\n';
  set a 12;
  print_int (get a);
  print_char '\n';
  print_char (get (bind a (fun x -> return 'a')));
  print_char '\n'
