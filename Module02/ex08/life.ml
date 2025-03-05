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
  | (_, _, x)::tail -> None::(generate_rna tail)

let rec print_rna r = match r with
  | [] -> print_char '\n'
  | A::tail -> print_char 'A'; print_rna tail
  | T::tail -> print_char 'T'; print_rna tail
  | C::tail -> print_char 'C'; print_rna tail
  | G::tail -> print_char 'G'; print_rna tail
  | U::tail -> print_char 'U'; print_rna tail
  | _::tail -> print_string "None"; print_rna tail

(* EX07 *)

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly
| His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val
type protein =  aminoacid list

let rec generate_base_triplets r = match r with
  | a::b::c::tail -> (a, b, c)::(generate_base_triplets tail)
  | _ -> []

let rec string_of_protein prot = match prot with
  | Stop::tail -> "(UAA, UAG, UGA) "^(string_of_protein tail)
  | Ala::tail -> "(GCA, GCC, GCG, GCU) "^(string_of_protein tail)
  | Arg::tail -> "(AGA, AGG, CGA, CGC, CGG, CGU) "^(string_of_protein tail)
  | Asn::tail -> "(AAC, AAU) "^(string_of_protein tail)
  | Asp::tail -> "(GAC, GAU) "^(string_of_protein tail)
  | Cys::tail -> "(UGC, UGU) "^(string_of_protein tail)
  | Gln::tail -> "(CAA, CAG) "^(string_of_protein tail)
  | Glu::tail -> "(GAA, GAG :"^(string_of_protein tail)
  | Gly::tail -> "(GGA, GGC, GGG, GGU) "^(string_of_protein tail)
  | His::tail -> "(CAC, CAU) "^(string_of_protein tail)
  | Ile::tail -> "(AUA, AUC, AUU) "^(string_of_protein tail)
  | Leu::tail -> "(CUA, CUC, CUG, CUU, UUA, UUG) "^(string_of_protein tail)
  | Lys::tail -> "(AAA, AAG) "^(string_of_protein tail)
  | Met::tail -> "(AUG) "^(string_of_protein tail)
  | Phe::tail -> "(UUC, UUU) "^(string_of_protein tail)
  | Pro::tail -> "(CCC, CCA, CCG, CCU) "^(string_of_protein tail)
  | Ser::tail -> "(UCA, UCC, UCG, UCU, AGU, AGC) "^(string_of_protein tail)
  | Thr::tail -> "(ACA, ACC, ACG, ACU) "^(string_of_protein tail)
  | Trp::tail -> "(UGG) "^(string_of_protein tail)
  | Tyr::tail -> "(UAC, UAU) "^(string_of_protein tail)
  | Val::tail -> "(GUA, GUC, GUG, GUU) "^(string_of_protein tail)
  | _ -> ""

let decode_arn r =
  let rec aux triplets = match triplets with
    | (U,A,A)::(U,A,G)::(U,G,A)::tail -> print_endline "Stop aminoacide found."; Stop::[]
    | (G,C,A)::(G,C,C)::(G,C,G)::(G,C,U)::tail -> print_endline "Ala aminoacide found."; Ala::(aux tail)
    | (A,G,A)::(A,G,G)::(C,G,A)::(C,G,C)::(C,G,G)::(C,G,U)::tail -> print_endline "Arg aminoacide found."; Arg::(aux tail)
    | (A,A,C)::(A,A,U)::tail -> print_endline "Asn aminoacide found."; Asn::(aux tail)
    | (G,A,C)::(G,A,U)::tail -> print_endline "Asp aminoacide found."; Asp::(aux tail)
    | (U,G,C)::(U,G,U)::tail -> print_endline "Cys aminoacide found."; Cys::(aux tail)
    | (C,A,A)::(C,A,G)::tail -> print_endline "Gln aminoacide found."; Gln::(aux tail)
    | (G,A,A)::(G,A,G)::tail -> print_endline "Glu aminoacide found."; Glu::(aux tail)
    | (G,G,A)::(G,G,C)::(G,G,G)::(G,G,U)::tail -> print_endline "Gly aminoacide found."; Gly::(aux tail)
    | (C,A,C)::(C,A,U)::tail -> print_endline "His aminoacide found."; His::(aux tail)
    | (A,U,A)::(A,U,C)::(A,U,U)::tail -> print_endline "Ile aminoacide found."; Ile::(aux tail)
    | (C,U,A)::(C,U,C)::(C,U,G)::(C,U,U)::(U,U,A)::(U,U,G)::tail -> print_endline "Leu aminoacide found."; Leu::(aux tail)
    | (A,A,A)::(A,A,G)::tail -> print_endline "Lys aminoacide found."; Lys::(aux tail)
    | (A,U,G)::tail -> print_endline "Met aminoacide found."; Met::(aux tail)
    | (U,U,C)::(U,U,U)::tail -> print_endline "Phe aminoacide found."; Phe::(aux tail)
    | (C,C,C)::(C,C,A)::(C,C,G)::(C,C,U)::tail -> print_endline "Pro aminoacide found."; Pro::(aux tail)
    | (U,C,A)::(U,C,C)::(U,C,G)::(U,C,U)::(A,G,U)::(A,G,C)::tail -> print_endline "Ser aminoacide found."; Ser::(aux tail)
    | (A,C,A)::(A,C,C)::(A,C,G)::(A,C,U)::tail -> print_endline "Thr aminoacide found."; Thr::(aux tail)
    | (U,G,G)::tail -> print_endline "Trp aminoacide found."; Trp::(aux tail)
    | (U,A,C)::(U,A,U)::tail -> print_endline "Tyr aminoacide found."; Tyr::(aux tail)
    | (G,U,A)::(G,U,C)::(G,U,G)::(G,U,U)::tail -> print_endline "Val aminoacide found."; Val::(aux tail)
    | _::tail -> aux tail
    | [] -> []
  in
  let rec rev_list prot = match prot with
    | head::tail -> head::(rev_list tail)
    | _ -> []
  in rev_list (aux (generate_base_triplets r))

(* EX08 *)

let life nucleotides_str =
  if nucleotides_str = "" then
    print_string "No nucleotides given.\n"
  else
    begin
      print_string "\nThe nucleobases are :\n";
      print_endline nucleotides_str;
      print_char '\n';
      let rec do_helix str i = match i with
        | j when j = String.length str -> []
        | _ -> (generate_nucleotide (String.get str i))::(do_helix str (i + 1))
      in
      let h = do_helix nucleotides_str 0 in
      print_string "The list of nucleotides generated is :\n";
      print_endline (helix_to_string h);
      print_char '\n';
      let r = generate_rna h in
      print_string "The rna generated is :\n";
      print_rna r;
      print_char '\n';
      let prot = decode_arn r in
      print_string "\nThe final protein is :\n";
      print_endline (string_of_protein prot)
    end

let () =
  print_endline "-------------------------- FIRST TEST --------------------------";
  life (helix_to_string (generate_helix 3000));
  print_endline "\n-------------------------- SECOND TEST --------------------------\n";
  life (helix_to_string (generate_helix (-1)))