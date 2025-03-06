let print_deck_string_list d =
  Printf.printf "[";
  let rec aux d = match d with
    | [] -> Printf.printf "]\n"
    | head::[] -> Printf.printf "%s" head; aux []
    | head::tail -> Printf.printf "%s, " head; aux tail
  in aux d

let print_draw (a, b) =
  Printf.printf "First is : %s\n" (Deck.Card.toStringVerbose a);
  print_deck_string_list (Deck.toStringList b)

let print_card_list l =
  Printf.printf "[";
  let rec aux l = match l with
    | [] -> Printf.printf "]\n"
    | head::[] -> Printf.printf "%s" (Deck.Card.toStringVerbose head); aux []
    | head::tail -> Printf.printf "%s, " (Deck.Card.toStringVerbose head); aux tail
  in aux l
  
let print_bool b = match b with
  | true -> "true"
  | false -> "false"

let main () =
  let deck1 = Deck.newDeck in

  print_deck_string_list (Deck.toStringListVerbose deck1);
  Printf.printf "\n";

  print_draw (Deck.drawCard deck1);
  Printf.printf "\n";

  let c1 = Deck.Card.newCard Deck.Card.Value.T10 Deck.Card.Color.Spade in
  let c2 = Deck.Card.newCard Deck.Card.Value.King Deck.Card.Color.Heart in

  print_card_list Deck.Card.allSpades;
  Printf.printf "\n";
  
  Printf.printf "getValue: %s\n" (Deck.Card.Value.toStringVerbose (Deck.Card.getValue c1));
  Printf.printf "getColor: %s\n" (Deck.Card.Color.toStringVerbose (Deck.Card.getColor c1));
  Printf.printf "\n";

  Printf.printf "max c1 c2: %s\n" (Deck.Card.toStringVerbose (Deck.Card.max c1 c2));
  Printf.printf "min c1 c2: %s\n" (Deck.Card.toStringVerbose (Deck.Card.min c1 c2));
  Printf.printf "compare c1 c2: %d\n" (Deck.Card.compare c1 c2);
  Printf.printf "best of all: %s\n" (Deck.Card.toStringVerbose (Deck.Card.best Deck.Card.all));
  Printf.printf "\n";

  Printf.printf "isOf: %s\n" (print_bool (Deck.Card.isOf c1 Deck.Card.Color.Club));
  Printf.printf "isSpade: %s\n" (print_bool (Deck.Card.isSpade c1));
  Printf.printf "isHeart: %s\n" (print_bool (Deck.Card.isHeart c1));
  Printf.printf "isDiamond: %s\n" (print_bool (Deck.Card.isDiamond c1));
  Printf.printf "isClub: %s\n" (print_bool (Deck.Card.isClub c1))

let () = main ()
