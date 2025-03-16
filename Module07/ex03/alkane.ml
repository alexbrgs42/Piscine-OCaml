let get_alkane_name_by_n n = match n with
  | 1 -> "methane"
  | 2 -> "ethane"
  | 3 -> "propane"
  | 4 -> "butane"
  | 5 -> "pentane"
  | 6 -> "hexane"
  | 7 -> "heptane"
  | 8 -> "octane"
  | 9 -> "nonane"
  | 10 -> "decane"
  | 11 -> "undecane"
  | _ -> "dodecane"

class virtual alkane n =
  object
    inherit Molecule.molecule (get_alkane_name_by_n n) (((new Atom.carbon)#generate_list n)@((new Atom.hydrogen)#generate_list (n * 2 + 2)))
  end

class methane =
  object
    inherit alkane 1
  end

class ethane =
  object
    inherit alkane 2
  end

class propane =
  object
    inherit alkane 3
  end

class butane =
  object
    inherit alkane 4
  end

class pentane =
  object
    inherit alkane 5
  end

class hexane =
  object
    inherit alkane 6
  end

class heptane =
  object
    inherit alkane 7
  end

class octane =
  object
    inherit alkane 8
  end

class nonane =
  object
    inherit alkane 9
  end

class decane =
  object
    inherit alkane 10
  end

class undecane =
  object
    inherit alkane 11
  end

class dodecane =
  object
    inherit alkane 12
  end
