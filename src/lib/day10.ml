open Base
open Utils.Matrix
open Utils.Tuple

let get_reachable_nines m z =
  let rec helper acc pos prev =
    let (x, y) = pos in
    try
      let v = m.(y).(x) in
      if v = prev + 1 then
        if v = 9
          then (pos :: acc)
          else
            List.concat
              [ helper acc (x, y - 1) v
              ; helper acc (x, y + 1) v
              ; helper acc (x - 1, y) v
              ; helper acc (x + 1, y) v
              ]
      else
        acc
    with
    | _ -> acc
  in helper [] z (-1)

let solve m =
  let keep_only_coord { c; value=_value } = c in

  let zs = m |> Utils.Matrix.find_points ~f:(fun x -> Option.some_if (x = 0) x)
    |> List.map ~f:keep_only_coord in

  let rec helper acc rem_zs =
    match rem_zs with
    | [] -> acc
    | z::rem_zs' ->
      let reached_ns = Set.of_list (module IntTuple) @@ get_reachable_nines m z in
      helper (acc + Set.length reached_ns) rem_zs'
  in helper 0 zs

let main rows =
  try
    rows
      |> Utils.Matrix.parse ~f:(fun c -> c |> Char.to_string |> Int.of_string)
      |> solve
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Failure _ -> Stdio.print_endline "Wrong character in the input file?"
