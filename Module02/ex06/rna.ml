(* EX04 *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = phosphate * deoxyribose * nucleobase

let generate_nucleotide nucleob = match nucleob with
  | 'A' -> ("phosphate", "deoxyribose", A)
  | 'T' -> ("phosphate", "deoxyribose", T)
  | 'C' -> ("phosphate", "deoxyribose", C)
  | 'G' -> ("phosphate", "deoxyribose", G)
  | _ -> ("phosphate", "deoxyribose", None)

(* EX05 *)

type helix = nucleotide list

let generate_helix n =
  if n <= 0 then
    []
  else
    let rec aux nb i = match nb with
      | _ when i = n -> []
      | 0 -> (generate_nucleotide 'A')::(aux (Random.int 4) (i + 1))
      | 1 -> (generate_nucleotide 'T')::(aux (Random.int 4) (i + 1))
      | 2 -> (generate_nucleotide 'C')::(aux (Random.int 4) (i + 1))
      | 3 -> (generate_nucleotide 'G')::(aux (Random.int 4) (i + 1))
      | _ -> (generate_nucleotide 'X')::(aux (Random.int 4) (i + 1))
    in aux (Random.int 4) 0

let rec helix_to_string h = match h with
  | [] -> ""
  | (_, _, A)::tail -> "A"^(helix_to_string tail)
  | (_, _, T)::tail -> "T"^(helix_to_string tail)
  | (_, _, C)::tail -> "C"^(helix_to_string tail)
  | (_, _, G)::tail -> "G"^(helix_to_string tail)
  | (_, _, _)::tail -> "None"^(helix_to_string tail)

let rec complementary_helix h = match h with
  | [] -> []
  | (a, b, A)::tail -> (a, b, T)::(complementary_helix tail)
  | (a, b, T)::tail -> (a, b, A)::(complementary_helix tail)
  | (a, b, C)::tail -> (a, b, G)::(complementary_helix tail)
  | (a, b, G)::tail -> (a, b, C)::(complementary_helix tail)
  | (a, b, _)::tail -> (a, b, None)::(complementary_helix tail)

(* EX06 *)

type rna = nucleobase list

let rec generate_rna h = match h with
  | [] -> []
  | (_, _, A)::tail -> U::(generate_rna tail)
  | (_, _, T)::tail -> A::(generate_rna tail)
  | (_, _, C)::tail -> G::(generate_rna tail)
  | (_, _, G)::tail -> C::(generate_rna tail)
  | (_, _, _)::tail -> None::(generate_rna tail)

let rec print_rna r = match r with
  | [] -> print_char '\n'
  | A::tail -> print_char 'A'; print_rna tail
  | T::tail -> print_char 'T'; print_rna tail
  | C::tail -> print_char 'C'; print_rna tail
  | G::tail -> print_char 'G'; print_rna tail
  | U::tail -> print_char 'U'; print_rna tail
  | _::tail -> print_string "None"; print_rna tail

let () =
  let h = generate_helix 20 in
  print_endline (helix_to_string h);
  print_rna (generate_rna h);