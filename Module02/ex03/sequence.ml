let sequence n =
  if n <= 0 then
    ""
  else
    let rec aux str acc c i = match str with
      | [] -> ((char_of_int i)::c::acc)
      | head::tail when head = c -> aux tail acc c (i + 1)
      | head::tail -> aux tail ((char_of_int i)::c::acc) head 1
    in aux "1" "" '1' 0

let () =
    print_endline (sequence 2)