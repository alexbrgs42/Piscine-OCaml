module type APP =
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

module App : APP = struct
  type project = string * string * int

  let zero = ("", "", 0)
  let combine project1 project2 = match project1, project2 with
   | (type1, status1, grade1), (type2, status2, grade2) ->
      let mean = (grade1 + grade2) / 2 in
      if mean >= 80 then
        (type1 ^ type2, "succeed", mean)
      else
        (type1 ^ type2, "failed", mean)
  let fail (project_type, _, _) = (project_type, "failed", 0)
  let success (project_type, _, _) = (project_type, "succeed", 80)
end

let print_proj ((t, s, g) : App.project) : unit =
  Printf.printf "Project: %s -> %s with grade %d\n" t s g

let main =
  let project1 = ("get_next_line", "succeed", 80) in
  let project2 = ("ft_printf", "failed", 75) in

  print_proj project1;
  print_proj project2;

  print_endline "";
  print_proj (App.combine project1 App.zero);
  print_proj (App.combine project1 project2);
  print_proj (App.fail project1);
  print_proj (App.success project2);
  print_proj (App.combine project1 (App.fail project1))
