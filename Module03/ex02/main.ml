let main () =
  let c1 = Card.newCard Card.Value.T10 Card.Color.Spade in
  let c2 = Card.newCard Card.Value.King Card.Color.Heart in

  List.iter (fun e -> print_string ((Card.toStringVerbose e)^(if e = (Card.Value.As, Card.Color.Spade) then "\n" else ", "))) Card.allSpades;
  
  print_endline (Printf.sprintf "getValue: %s" (Card.Value.toStringVerbose (Card.getValue c1)));
  print_endline (Printf.sprintf "getColor: %s" (Card.Color.toStringVerbose (Card.getColor c1)));

  print_endline (Printf.sprintf "max c1 c2: %s" (Card.toStringVerbose (Card.max c1 c2)));
  print_endline (Printf.sprintf "min c1 c2: %s" (Card.toStringVerbose (Card.min c1 c2)));
  print_endline (Printf.sprintf "compare c1 c2: %d" (Card.compare c1 c2));
  print_endline (Printf.sprintf "best of all: %s" (Card.toStringVerbose (Card.best Card.all)));

  print_endline (Printf.sprintf "isOf: %B" (Card.isOf c1 Card.Color.Club));
  print_endline (Printf.sprintf "isSpade: %B" (Card.isSpade c1));
  print_endline (Printf.sprintf "isHeart: %B" (Card.isHeart c1));
  print_endline (Printf.sprintf "isDiamond: %B" (Card.isDiamond c1));
  print_endline (Printf.sprintf "isClub: %B" (Card.isClub c1))

let () = main ()
