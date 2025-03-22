type exn += Exc of string

module type TRY =
  sig
    type 'a t = Success of 'a | Failure of exn
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val flatten : 'a t t -> 'a t
    val get_type : 'a t -> string
    val get_value : 'a t -> 'a
    val get_exn : exn t -> string
  end

module Try : TRY = struct
  type 'a t = Success of 'a | Failure of exn
  let return x = Success x
  let bind my_try f = match my_try with
    | Success x -> (try f x with e -> Failure e)
    | Failure e -> Failure e
  let recover my_try f = match my_try with
    | Success _ -> my_try
    | Failure e -> f e
  let filter my_try f = match my_try with
    | Success x when f x = false -> Failure (Exc "failed predicate")
    | _ -> my_try
  let flatten my_try = match my_try with
    | Success (Success x) -> Success x
    | Success (Failure x) -> Failure x
    | Failure _ -> failwith "Impossible pattern."
  let get_type my_try = match my_try with
    | Success x -> "Success"
    | Failure x -> "Failure"
  let get_exn my_try = match my_try with
    | Failure x -> Printexc.to_string x
    | _ -> failwith "Must pass a Success as argument."
  let get_value my_try = match my_try with
    | Success x -> x
    | _ -> failwith "Must pass a Success as argument."
  end

module type MONAD = 
  functor (Try : TRY) ->
    sig
      val return : 'a -> 'a Try.t
      val bind : 'a Try.t -> ('a -> 'b Try.t) -> 'b Try.t
      val recover : 'a Try.t -> (exn -> 'a Try.t) -> 'a Try.t
      val filter : 'a Try.t -> ('a -> bool) -> 'a Try.t
      val flatten : 'a Try.t Try.t -> 'a Try.t
      val get_type : 'a Try.t -> string
      val get_value : 'a Try.t -> 'a
      val get_exn : exn Try.t -> string
  end

module Monad : MONAD =
  functor (Try : TRY) ->
    struct
      let return = Try.return
      let bind = Try.bind
      let recover = Try.recover
      let filter = Try.filter
      let flatten = Try.flatten
      let get_type = Try.get_type
      let get_exn = Try.get_exn
      let get_value = Try.get_value
    end

module MyMonad = Monad (Try)

let () =
  let success = MyMonad.return 1 in
  let failure = MyMonad.bind success (fun x -> Try.Failure (Exc "binding")) in
  let recover = MyMonad.recover failure (fun x -> Try.Failure (Exc "recovering")) in
  let filter = MyMonad.filter success (fun x -> x mod 2 = 0) in
  let success2 = MyMonad.return success in
  let flatten_success = MyMonad.flatten success2 in

  print_endline ((MyMonad.get_type success) ^ " of " ^ (string_of_int (MyMonad.get_value success)));
  print_endline ((MyMonad.get_type failure) ^ " of " ^ (MyMonad.get_exn failure));
  print_endline ((MyMonad.get_type recover) ^ " of " ^ (MyMonad.get_exn recover));
  match filter with
    | Success x -> print_endline ("Success of " ^ (string_of_int x))
    | Failure x -> print_endline ("Failure of " ^ (Printexc.to_string x));
  print_endline ((MyMonad.get_type (MyMonad.get_value success2)) ^ " of " ^ (string_of_int (MyMonad.get_value (MyMonad.get_value success2))));
  print_endline ((MyMonad.get_type flatten_success) ^ " of " ^ (string_of_int (MyMonad.get_value flatten_success)));
