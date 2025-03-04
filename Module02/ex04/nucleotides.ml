type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleob = match nucleob with
  | 'A' -> ("phosphate", "deoxyribose", A)
  | 'T' -> ("phosphate", "deoxyribose", T)
  | 'C' -> ("phosphate", "deoxyribose", C)
  | 'G' -> ("phosphate", "deoxyribose", G)
  | _ -> ("phosphate", "deoxyribose", None)

let print_nucleotide nucleot = match nucleot with
  | (a, b, c) -> print_string "("; print_string a; print_string ", "; print_string b; print_string ", ";
  match c with
    | A -> print_string "A)\n"
    | T -> print_string "T)\n"
    | C -> print_string "C)\n"
    | G -> print_string "G)\n"
    | None -> print_string "None)\n"

let () =
  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'Z')
