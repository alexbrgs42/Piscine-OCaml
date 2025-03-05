type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString card_color = match card_color with
  | Spade -> "S"
  | Heart -> "H"
  | Diamond -> "D"
  | Club -> "C"

let toStringVerbose card_color = match card_color with
  | Spade -> "Spade"
  | Heart -> "Heart"
  | Diamond -> "Diamond"
  | Club -> "Club"
