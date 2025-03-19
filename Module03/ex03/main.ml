let print_draw (a, b) =
  print_endline ("First is : " ^ (Deck.Card.toStringVerbose a));
  print_endline "Rest of deck :";
  List.iter (fun e -> print_string (e ^ ", ")) (Deck.toStringList b)

let main () =
  let deck1 = Deck.newDeck in
  let c1 = Deck.Card.newCard Deck.Card.Value.T10 Deck.Card.Color.Spade in
  let c2 = Deck.Card.newCard Deck.Card.Value.King Deck.Card.Color.Heart in

  List.iter (fun e -> print_string (e ^ ", ")) (Deck.toStringListVerbose deck1);
  print_endline "\n";

  print_draw (Deck.drawCard deck1);
  print_endline "\n";

  print_endline "All Spades :";
  List.iter (fun e -> print_string ((Deck.Card.toStringVerbose e) ^ (if (Deck.Card.getValue e) = Deck.Card.Value.As then "\n" else ", "))) Deck.Card.allSpades;
  print_endline "";
  
  print_endline ("getValue Card(T10, Spade) : " ^ (Deck.Card.Value.toStringVerbose (Deck.Card.getValue c1)));
  print_endline ("getColor Card(T10, Spade) : " ^ (Deck.Card.Color.toStringVerbose (Deck.Card.getColor c1)));
  print_endline "";

  print_endline ("max c1 c2: " ^ (Deck.Card.toStringVerbose (Deck.Card.max c1 c2)));
  print_endline ("min c1 c2: " ^ (Deck.Card.toStringVerbose (Deck.Card.min c1 c2)));
  print_endline ("compare c1 c2: " ^ (string_of_int (Deck.Card.compare c1 c2)));
  print_endline ("best of all: " ^ (Deck.Card.toStringVerbose (Deck.Card.best Deck.Card.all)));
  print_endline "";

  print_endline (Printf.sprintf "isOf: %B" (Deck.Card.isOf c1 Deck.Card.Color.Club));
  print_endline (Printf.sprintf "isSpade: %B" (Deck.Card.isSpade c1));
  print_endline (Printf.sprintf "isHeart: %B" (Deck.Card.isHeart c1));
  print_endline (Printf.sprintf "isDiamond: %B" (Deck.Card.isDiamond c1));
  print_endline (Printf.sprintf "isClub: %B" (Deck.Card.isClub c1))

let () = main ()
