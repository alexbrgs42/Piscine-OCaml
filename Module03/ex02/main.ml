let main () =
  let c1 = Card.newCard Card.Value.T10 Card.Color.Spade in
  let c2 = Card.newCard Card.Value.King Card.Color.Heart in

  List.iter (fun e -> print_string ((Card.toStringVerbose e)^(if e = (Card.Value.As, Card.Color.Spade) then "\n" else ", "))) Card.allSpades;
  
  Printf.printf "getValue: %s\n" (Card.Value.toStringVerbose (Card.getValue c1));
  Printf.printf "getColor: %s\n" (Card.Color.toStringVerbose (Card.getColor c1));

  Printf.printf "max c1 c2: %s\n" (Card.toStringVerbose (Card.max c1 c2));
  Printf.printf "min c1 c2: %s\n" (Card.toStringVerbose (Card.min c1 c2));
  Printf.printf "compare c1 c2: %d\n" (Card.compare c1 c2);
  Printf.printf "best of all: %s\n" (Card.toStringVerbose (Card.best Card.all));

  Printf.printf "isOf: %B\n" (Card.isOf c1 Card.Color.Club);
  Printf.printf "isSpade: %B\n" (Card.isSpade c1);
  Printf.printf "isHeart: %B\n" (Card.isHeart c1);
  Printf.printf "isDiamond: %B\n" (Card.isDiamond c1);
  Printf.printf "isClub: %B\n" (Card.isClub c1)

let () = main ()
