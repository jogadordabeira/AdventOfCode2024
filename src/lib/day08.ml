open Base
open Utils.Matrix
open Utils.Tuple

let get_antinode len (x1, y1) (x2, y2) =
  let dist_x = Int.abs (x1 - x2) and dist_y = Int.abs (y1 - y2) in

  let x3 = if x1 >= x2 then (x1 + dist_x) else (x1 - dist_x) in
  let x4 = if x1 >= x2 then (x2 - dist_x) else (x2 + dist_x) in

  let y3 = if y1 >= y2 then (y1 + dist_y) else (y1 - dist_y) in
  let y4 = if y1 >= y2 then (y2 - dist_y) else (y2 + dist_y) in
  
  [(x3, y3); (x4, y4)]
    |> List.filter ~f:(fun (x, y) -> 0 <= x && x < len && 0 <= y && y < len)

let get_antinodes len antennas =
  try
    let cs = antennas |> List.map ~f:(fun { c; value = _value } -> c) in
    List.cartesian_product cs cs
      |> List.concat_map ~f:(fun (c1, c2) ->
          if IntTuple.equal c1 c2 then [] else get_antinode len c1 c2)
  with _ -> []

let solve len pts =
  pts
    |> List.sort ~compare:(fun { c = _c1; value = v1 } { c = _c2; value = v2 } -> Char.compare v1 v2)
    |> List.group ~break:(fun { c = _c1; value = v1 } { c = _c2; value = v2 } -> not @@ Char.equal v1 v2)
    |> List.concat_map ~f:(get_antinodes len)
    |> List.dedup_and_sort ~compare:IntTuple.compare
    |> List.length

let main rows =
  let m = rows |> Utils.Matrix.parse ~f:Fn.id in
  let len = Array.length m in
  m
    |> Utils.Matrix.find_points ~f:(fun c -> Option.some_if (not (Char.equal c '.')) c)
    |> solve len (* we expect a square *)
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline
