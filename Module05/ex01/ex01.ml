module FletcherHash : (Hashtbl.HashedType with type t = string) = struct
  type t = string
  let equal str1 str2 = str1 = str2
  let hash str =
    let rec aux i acc = match i with
      | -1 -> acc
      | _ -> aux (i - 1) ((acc + int_of_char (String.get str i)) mod 255)
    in
    aux (String.length str - 1) 0
end

module StringHashtbl : (Hashtbl.S with type key = string) = Hashtbl.Make (FletcherHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
