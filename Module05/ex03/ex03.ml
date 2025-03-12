module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig
    val bits : int
  end

module type MAKE =
  functor (FractBits : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
  functor (FractBits : FRACTIONNAL_BITS) ->
    struct
      type t = int
      let roundf v = floor (v +. 0.5)
      let of_float v = int_of_float (roundf (v *. (2. ** float_of_int FractBits.bits)))
      let of_int v = v * int_of_float (2. ** float_of_int FractBits.bits)
      let to_float v = (float_of_int v) /. (2. ** float_of_int FractBits.bits)
      let to_int v = v / int_of_float (2. ** float_of_int FractBits.bits)
      let to_string v = string_of_float (to_float v)
      let zero = 0
      let one = of_int 1
      let succ = Stdlib.succ
      let pred = Stdlib.pred
      let min = Stdlib.min
      let max = Stdlib.max
      let gth = ( > )
      let lth = ( < )
      let gte = ( >= )
      let lte = ( <= )
      let eqp = ( == )
      let eqs = ( = )
      let add = ( + )
      let sub = ( - )
      let mul = ( * )
      let div = ( / )
      let foreach a b f =
        let rec aux x = match x with
          | y when y = b -> f b
          | _ -> f x; aux (succ x)
        in aux a
    end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))

  (* ADD TESTS *)
  (* TEST IF x * y / y = x *)
