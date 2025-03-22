module type MONOID =
  sig
    type element
    val zero1 : element
    val zero2 : element
    val add : element -> element -> element
    val sub : element -> element -> element
    val mul : element -> element -> element
    val div : element -> element -> element
  end

module INT : (MONOID with type element = int) =
  struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let add = ( + )
    let sub = ( - )
    let mul = ( * )
    let div e1 e2 =
      if e2 = 0 then
        failwith "Division by 0 is illegal."
      else
        e1 / e2
  end

module FLOAT : (MONOID with type element = float) =
  struct
    type element = float
    let zero1 = 0.
    let zero2 = 1.
    let add = ( +. )
    let sub = ( -. )
    let mul = ( *. )
    let div e1 e2 =
      if e2 = 0. then
        failwith "Division by 0 is illegal."
      else
        e1 /. e2
  end

module type CALC =
  functor (M : MONOID) ->
    sig
      val add : M.element -> M.element -> M.element
      val sub : M.element -> M.element -> M.element
      val mul : M.element -> M.element -> M.element
      val div : M.element -> M.element -> M.element
      val power : M.element -> int -> M.element
      val fact : M.element -> M.element
    end

module Calc : CALC =
  functor (M : MONOID) -> struct
    let add = M.add
    let sub = M.sub
    let mul = M.mul
    let div = M.div
    let power x n =
      if n < 0 then
        failwith "Exponant must be positive."
      else
        let rec aux i acc = match i with
          | j when j = n -> acc
          | _ -> aux (i + 1) (M.mul x acc)
        in aux 0 M.zero2
    let fact n =
      if n < M.zero1 then
        failwith "Factorial must be positive."
      else
        let rec aux i acc = match i with
          | j when j = (M.add n M.zero2) -> acc
          | _ -> aux (M.add i M.zero2) (M.mul acc i)
        in aux M.zero2 M.zero2
end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
print_endline (string_of_int (Calc_int.power 3 3));
print_endline (string_of_int (Calc_int.power 3 0));
print_endline (string_of_float (Calc_float.power 3.0 3));
print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
print_endline (string_of_int (Calc_int.fact 4));
print_endline (string_of_float (Calc_float.fact 4.));
try
  print_endline (string_of_float (Calc_float.fact (-4.)))
with e -> print_endline (Printexc.to_string e);
try
  print_endline (string_of_int (Calc_int.fact (-4)))
with e -> print_endline (Printexc.to_string e);
try
  print_endline (string_of_float (Calc_float.power 3. (-4)))
with e -> print_endline (Printexc.to_string e);
try
  print_endline (string_of_int (Calc_int.power 3 (-1)))
with e -> print_endline (Printexc.to_string e);
try
  print_endline (string_of_float (Calc_float.div 2. 0.));
with e -> print_endline (Printexc.to_string e);
try
  print_endline (string_of_int (Calc_int.div 10 0));
with e -> print_endline (Printexc.to_string e);
print_endline (string_of_int (Calc_int.sub 10 4));
print_endline (string_of_float (Calc_float.sub 4. 32.))
