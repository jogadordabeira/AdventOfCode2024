open Base

let parse_rows row =
  row
    |> String.split ~on:' '
    |> List.filter_map ~f:Utils.Int.int_of_string_or_none

(* there's definitely a smarter way to do this *)
let is_safe limit xs =
  let ds =
    xs
      |> Utils.List.pair
      |> List.map ~f:(fun (x, y) -> x - y) in
  let is_all_asc = ds |> List.for_all ~f:(fun x -> x < 0) in
  let is_all_desc = ds |> List.for_all ~f:(fun x -> x > 0) in
  let is_between_limits =
    ds
      |> List.for_all ~f:(fun x -> let d = Int.abs x in 0 < d && d <= limit) in
  (is_all_asc || is_all_desc) && is_between_limits

let main rows =
  rows
    |> List.count ~f:(fun r -> r |> parse_rows |> is_safe 3)
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline

(* Some testing *)
let%test "is_safe" = is_safe 3 [7; 6; 4; 2; 1]
let%test "is_safe" = not @@ is_safe 3 [1; 2; 7; 8; 9]
let%test "is_safe" = not @@ is_safe 3 [9; 7; 6; 2; 1]
let%test "is_safe" = not @@ is_safe 3 [1; 3; 2; 4; 5]
let%test "is_safe" = not @@ is_safe 3 [8; 6; 4; 4; 1]
let%test "is_safe" = is_safe 3 [1; 3; 6; 7; 9]
