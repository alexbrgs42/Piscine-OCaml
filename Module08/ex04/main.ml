module type SET =
  sig
    type 'a t = 'a list
    val return: 'a -> 'a t
    val create: 'a list -> 'a t
    val to_string_int: int t -> string
    val to_string_string: string t -> string
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val union: 'a t -> 'a t -> 'a t
    val inter: 'a t -> 'a t -> 'a t
    val diff: 'a t -> 'a t -> 'a t
    val filter: 'a t -> ('a -> bool) -> 'a t
    val foreach: 'a t -> ('a -> unit) -> unit
    val for_all: 'a t -> ('a -> bool) -> bool
    val exists: 'a t -> ('a -> bool) -> bool
  end

module type MONAD =
  functor (Set : SET) ->
    sig
      val return: 'a -> 'a Set.t
      val create: 'a list -> 'a Set.t
      val to_string_int: int Set.t -> string
      val to_string_string: string Set.t -> string
      val bind: 'a Set.t -> ('a -> 'b Set.t) -> 'b Set.t
      val union: 'a Set.t -> 'a Set.t -> 'a Set.t
      val inter: 'a Set.t -> 'a Set.t -> 'a Set.t
      val diff: 'a Set.t -> 'a Set.t -> 'a Set.t
      val filter: 'a Set.t -> ('a -> bool) -> 'a Set.t
      val foreach: 'a Set.t -> ('a -> unit) -> unit
      val for_all: 'a Set.t -> ('a -> bool) -> bool
      val exists: 'a Set.t -> ('a -> bool) -> bool
    end

module Set : SET =
  struct
    type 'a t = 'a list
    let return x = [x]
    let create lst = lst
    let rec bind set f = match set with
      | head::tail -> (f head)@(bind tail f)
      | [] -> []
    let rec to_string_int set = match set with
      | [] -> "empty"
      | head::[] -> string_of_int head
      | head::tail -> (string_of_int head) ^ ", " ^ (to_string_int tail)
    let rec to_string_string set = match set with
      | [] -> "empty"
      | head::[] -> "\"" ^ head ^ "\""
      | head::tail -> "\"" ^ head ^ "\"" ^ ", " ^ (to_string_string tail)
    let union set1 set2 = set1@set2
    let rec filter set f = match set with
      | head::tail when f head -> head::(filter tail f)
      | _::tail -> filter tail f
      | [] -> []
    let inter set1 set2 =
      let rec aux s1 = match s1 with
        | head::tail -> (filter set2 (fun x -> x = head))@(aux tail)
        | [] -> []
      in aux set1
    let diff set1 set2 =
      let rec aux set comp_set = match set with
        | head::tail when filter comp_set (fun x -> x = head) = [] -> head::(aux tail comp_set)
        | head::tail -> aux tail comp_set
        | [] -> []
      in union (aux set1 set2) (aux set2 set1)
    let rec foreach set f = match set with
      | head::tail -> f head; foreach tail f
      | [] -> ()
    let rec for_all set f = match set with
      | head::tail -> (f head) && (for_all tail f)
      | [] -> true
    let rec exists set f = match set with
    | head::tail -> (f head) || (exists tail f)
    | [] -> false
  end

module Monad : MONAD =
  functor (Set : SET) ->
    struct
      let return = Set.return
      let create = Set.create
      let to_string_int = Set.to_string_int
      let to_string_string = Set.to_string_string
      let bind = Set.bind
      let union = Set.union
      let inter = Set.inter
      let diff = Set.diff
      let filter = Set.filter
      let foreach = Set.foreach
      let for_all = Set.for_all
      let exists = Set.exists
    end

module MySet = Monad (Set)

let () =
  let singleton = MySet.return "A" in
  let set1 = MySet.create [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let set2 = MySet.create [10; 11; 12; 13; 14; 15; 16; 17; 18; 19] in

  print_endline ("singleton : " ^ (MySet.to_string_string singleton));
  print_endline ("set1 : " ^ (MySet.to_string_int set1));
  print_endline ("set2 : " ^ (MySet.to_string_int set2));
  print_endline ("bind set1 : " ^ (MySet.to_string_string (MySet.bind set1 (fun x -> MySet.return "Hello"))));
  print_endline ("union set1 set2 : " ^ (MySet.to_string_int (MySet.union set1 set2)));
  print_endline ("inter set1 set2 : " ^ (MySet.to_string_int (MySet.inter set1 set2)));
  print_endline ("inter set1 [2] : " ^ (MySet.to_string_int (MySet.inter set1 (MySet.return 2))));
  print_endline ("diff set1 set2 : " ^ (MySet.to_string_int (MySet.diff set1 set2)));
  print_endline ("filter set1 is_pair : " ^ (MySet.to_string_int (MySet.filter set1 (fun x -> x mod 2 = 0))));
  MySet.foreach set2 print_int;
  print_endline "";
  if (MySet.for_all set1 (fun x -> x mod 2 = 0)) = true then
    print_endline ("for_all set1 is_pair : true")
  else
    print_endline ("for_all set1 is_pair : false");
  if (MySet.for_all set1 (fun x -> x > 0)) = true then
    print_endline ("for_all set1 is_positive : true")
  else
    print_endline ("for_all set1 is_positive : false");
  if (MySet.exists set1 (fun x -> x mod 2 = 0)) = true then
    print_endline ("exists set1 is_pair : true")
  else
    print_endline ("exists set1 is_pair : false");
  if (MySet.exists set1 (fun x -> x < 0)) = true then
    print_endline ("exists set1 is_negative : true")
  else
    print_endline ("exists set1 is_negative : false")
