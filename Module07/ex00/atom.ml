class virtual atom name symbol atomic_number =
  object (self)
    method get_name : string = name
    method get_symbol : string = symbol
    method get_atomic_number : int = atomic_number

    method to_string = "Atom: { name=" ^ self#get_name ^
                        ", symbol=" ^ self#get_symbol ^
                        ", atomic_number=" ^ (string_of_int self#get_atomic_number) ^
                        " }"
    method equals (that:atom) =
      that#get_name = self#get_name &&
      that#get_symbol = self#get_symbol &&
      that#get_atomic_number = self#get_atomic_number
  end

class hydrogen =
  object
    inherit atom "hydrogen" "H" 1
  end

class carbon =
  object
    inherit atom "carbon" "C" 6
  end

class oxygen =
  object
    inherit atom "oxygen" "O" 8
  end

class nitrogen =
  object
    inherit atom "nitrogen" "N" 7
  end

class helium =
  object
    inherit atom "helium" "He" 2
  end

class sulfur =
  object
    inherit atom "sulfur" "S" 16
  end
