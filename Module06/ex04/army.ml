class ['a] army =
  object (self)
    val members : 'a list = []
    
    method add (instance :'a) = {< members = List.append members [instance] >}
    method delete =
      let rec aux lst = match lst with
        | [] | _::[] -> []
        | hd::tl -> hd::(aux tl)
      in
      {< members = aux members >}
    method get_members = members
  end
