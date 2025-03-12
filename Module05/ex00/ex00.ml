module StringOrder = 
  struct
    type t = string
    let compare str1 str2 =
      if str1 > str2 then
        1
      else if str1 = str2 then
        0
      else
        (-1)
  end

module StringSet = Set.Make (StringOrder)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")
