module Color = struct
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
end

module Value = struct
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

  let toInt card_value = match card_value with
    | T2 -> 1
    | T3 -> 2
    | T4 -> 3
    | T5 -> 4
    | T6 -> 5
    | T7 -> 6
    | T8 -> 7
    | T9 -> 8
    | T10 -> 9
    | Jack -> 10
    | Queen  -> 11
    | King -> 12
    | As -> 13

  let toString card_value = match card_value with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "J"
    | Queen  -> "Q"
    | King -> "K"
    | As -> "A"

  let toStringVerbose card_value = match card_value with
    | T2 -> "2"
    | T3 -> "3"
    | T4 -> "4"
    | T5 -> "5"
    | T6 -> "6"
    | T7 -> "7"
    | T8 -> "8"
    | T9 -> "9"
    | T10 -> "10"
    | Jack -> "Jack"
    | Queen  -> "Queen"
    | King -> "King"
    | As -> "As"

  let next card_value = match card_value with
    | T2 -> T3
    | T3 -> T4
    | T4 -> T5
    | T5 -> T6
    | T6 -> T7
    | T7 -> T8
    | T8 -> T9
    | T9 -> T10
    | T10 -> Jack
    | Jack -> Queen
    | Queen  -> King
    | King -> As
    | As -> invalid_arg "As has no next value"

  let previous card_value = match card_value with
    | T2 -> invalid_arg "T2 has no previous value"
    | T3 -> T2
    | T4 -> T3
    | T5 -> T4
    | T6 -> T5
    | T7 -> T6
    | T8 -> T7
    | T9 -> T8
    | T10 -> T9
    | Jack -> T10
    | Queen  -> Jack
    | King -> Queen
    | As -> King
end

type t = Value.t * Color.t

let newCard (value: Value.t)  (color: Color.t) : t =
  (value, color)

let allSpades = 
  let rec aux value = match value with
    | Value.As -> [(newCard Value.As Color.Spade)]
    | v -> (newCard v Color.Spade)::(aux (Value.next v))
  in aux Value.T2

let allHearts = 
  let rec aux value = match value with
    | Value.As -> [(newCard Value.As Color.Heart)]
    | v -> (newCard v Color.Heart)::(aux (Value.next v))
  in aux Value.T2

let allDiamonds = 
  let rec aux value = match value with
    | Value.As -> [(newCard Value.As Color.Diamond)]
    | v -> (newCard v Color.Diamond)::(aux (Value.next v))
  in aux Value.T2

let allClubs = 
  let rec aux value = match value with
    | Value.As -> [(newCard Value.As Color.Club)]
    | v -> (newCard v Color.Club)::(aux (Value.next v))
  in aux Value.T2

let all = allSpades@allHearts@allDiamonds@allClubs

let getValue ((value, c): t) =
  value

let getColor ((v, color): t) =
  color

let toString ((value, color): t) =
  (Value.toString value)^(Color.toString color)

let toStringVerbose ((value, color): t) =
  "Card("^(Value.toStringVerbose value)^", "^(Color.toStringVerbose color)^")"

let compare ((v1, c1): t) ((v2, c2): t) = match v1, v2 with
  | a, b when (Value.toInt a) > (Value.toInt b) -> 1
  | a, b when (Value.toInt a) < (Value.toInt b) -> (-1)
  | _ -> 0  

let max (card1: t) (card2: t) : t  = match compare card1 card2 with
  | 1 | 0 -> card1
  | _ -> card2

let min (card1: t) (card2: t) : t = match compare card1 card2 with
  | 1 -> card2
  | _ -> card1

let best (card_list: t list) : t =
  if card_list = [] then
    invalid_arg "List is empty."
  else
    List.fold_left max (Value.T2, Color.Spade) card_list

let isOf (card: t) (ref_color: Color.t) = match card with
  | (v, c) -> c = ref_color

let isSpade ((v, c): t) =
  c = Color.Spade

let isHeart ((v, c): t) =
  c = Color.Heart

let isDiamond ((v, c): t) =
  c = Color.Diamond

let isClub ((v, c): t) =
  c = Color.Club
