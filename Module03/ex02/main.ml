let print_card_list l =
  Printf.printf "[";
  let rec aux l = match l with
    | [] -> Printf.printf "]\n"
    | head::[] -> Printf.printf "%s" (Card.toStringVerbose head); aux []
    | head::tail -> Printf.printf "%s, " (Card.toStringVerbose head); aux tail
  in aux l

let print_bool b = match b with
  | true -> "true"
  | false -> "false"

let main () =
  let c1 = Card.newCard Card.Value.T10 Card.Color.Spade in
  let c2 = Card.newCard Card.Value.King Card.Color.Heart in

  print_card_list Card.allSpades;
  
  Printf.printf "getValue: %s\n" (Card.Value.toStringVerbose (Card.getValue c1));
  Printf.printf "getColor: %s\n" (Card.Color.toStringVerbose (Card.getColor c1));

  Printf.printf "max c1 c2: %s\n" (Card.toStringVerbose (Card.max c1 c2));
  Printf.printf "min c1 c2: %s\n" (Card.toStringVerbose (Card.min c1 c2));
  Printf.printf "compare c1 c2: %d\n" (Card.compare c1 c2);
  Printf.printf "best of all: %s\n" (Card.toStringVerbose (Card.best Card.all));

  Printf.printf "isOf: %s\n" (print_bool (Card.isOf c1 Card.Color.Club));
  Printf.printf "isSpade: %s\n" (print_bool (Card.isSpade c1));
  Printf.printf "isHeart: %s\n" (print_bool (Card.isHeart c1));
  Printf.printf "isDiamond: %s\n" (print_bool (Card.isDiamond c1));
  Printf.printf "isClub: %s\n" (print_bool (Card.isClub c1))

let () = main ()
