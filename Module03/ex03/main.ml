let print_draw (a, b) =
  Printf.printf "First is : %s\n" (Deck.Card.toStringVerbose a);
  List.iter (fun e -> Printf.printf "%s%s" e (if e = "AC" then "\n" else ", ")) (Deck.toStringList b)

let main () =
  let deck1 = Deck.newDeck in

  List.iter (fun e -> Printf.printf "%s%s" e (if e = "Card(As, Club)" then "\n" else ", ")) (Deck.toStringListVerbose deck1);
  Printf.printf "\n";

  print_draw (Deck.drawCard deck1);
  Printf.printf "\n";

  let c1 = Deck.Card.newCard Deck.Card.Value.T10 Deck.Card.Color.Spade in
  let c2 = Deck.Card.newCard Deck.Card.Value.King Deck.Card.Color.Heart in

  List.iter (fun e -> Printf.printf "%s%s" (Deck.Card.toStringVerbose e) (if (Deck.Card.getValue e) = Deck.Card.Value.As then "\n" else ", ")) Deck.Card.allSpades;
  Printf.printf "\n";
  
  Printf.printf "getValue: %s\n" (Deck.Card.Value.toStringVerbose (Deck.Card.getValue c1));
  Printf.printf "getColor: %s\n" (Deck.Card.Color.toStringVerbose (Deck.Card.getColor c1));
  Printf.printf "\n";

  Printf.printf "max c1 c2: %s\n" (Deck.Card.toStringVerbose (Deck.Card.max c1 c2));
  Printf.printf "min c1 c2: %s\n" (Deck.Card.toStringVerbose (Deck.Card.min c1 c2));
  Printf.printf "compare c1 c2: %d\n" (Deck.Card.compare c1 c2);
  Printf.printf "best of all: %s\n" (Deck.Card.toStringVerbose (Deck.Card.best Deck.Card.all));
  Printf.printf "\n";

  Printf.printf "isOf: %B\n" (Deck.Card.isOf c1 Deck.Card.Color.Club);
  Printf.printf "isSpade: %B\n" (Deck.Card.isSpade c1);
  Printf.printf "isHeart: %B\n" (Deck.Card.isHeart c1);
  Printf.printf "isDiamond: %B\n" (Deck.Card.isDiamond c1);
  Printf.printf "isClub: %B\n" (Deck.Card.isClub c1)

let () = main ()
