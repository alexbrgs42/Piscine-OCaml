module Watchtower = struct
  type hour = int
  let zero = 12
  let add h1 h2 = (h1 + h2) mod 12
  let sub h1 h2 = (h1 - h2) mod 12
end

let () =
  let hour1 : Watchtower.hour = 7 in
  let hour2 : Watchtower.hour = 10 in

  Printf.printf "7 + 10 : %d\n" (Watchtower.add hour1 hour2);
  Printf.printf "10 - 7 : %d\n" (Watchtower.sub hour2 hour1);
  Printf.printf "10 + zero : %d\n" (Watchtower.add hour2 Watchtower.zero);
  Printf.printf "zero + 10 : %d\n" (Watchtower.add Watchtower.zero hour2)
