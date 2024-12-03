open Base

let parse r = 
  try
    let x = Re.Group.get r 1 |> Int.of_string in
    let y = Re.Group.get r 2 |> Int.of_string in
    Some (x, y)
  with _ -> None

let parse_all s =
  let regex = Re.compile @@ Re.Pcre.re "mul\\(([0-9]{1,3})\\,([0-9]{1,3})\\)" in
  Re.all regex (String.strip s) |> List.filter_map ~f:parse

let solve s =
  parse_all s |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + (x * y))

let main rows =
  String.concat rows
    |> solve
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline

(* Some testing *)
let%test "Solve" =
  solve "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" = 161

let%test "Solve (2)" =
  solve "mul(0,0)" = 0

let%test "Solve (3)" =
  solve "mul(10,1)" = 10

let%test "Solve (4)" =
  solve "mul(100,1)" = 100

let%test "Solve (5)" =
  solve "mul(1000,1)" = 0
