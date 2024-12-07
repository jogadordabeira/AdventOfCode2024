open Base
open Utils.Matrix

module Direction = struct
  type t = North | East | South | West

  let parse c =
    match c with
    | '^' -> Some North
    | '>' -> Some East
    | 'v' -> Some South
    | '<' -> Some West
    | _   -> None
  
  let move d =
    match d with
    | North -> fun (x, y) -> (x, y - 1)
    | East  -> fun (x, y) -> (x + 1, y)
    | South -> fun (x, y) -> (x, y + 1)
    | West  -> fun (x, y) -> (x - 1, y)

  let%test "Move" =
    Utils.Tuple.equal_tuple' Int.compare (move East (0, 0)) (1, 0)

  let next d =
    (* probably derive enum and do something with it here instead *)
    match d with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North
end

exception Unknown_character_found of char

let take_until m starting_point starting_dir: 'a list =
  let rec helper acc current_point current_dir =
    let acc' = (current_point :: acc) in
    let next_point = Direction.move current_dir current_point in
    let (next_x, next_y) = next_point in
    try
      let v = m.(next_y).(next_x) in
      match v with
      | '#' -> helper acc' current_point (Direction.next current_dir)
      | _ -> helper acc' (Direction.move current_dir current_point) current_dir
    with
    | Invalid_argument _s -> acc' (* end case *)
  in helper [] starting_point starting_dir

let solve m =
  let { c = first_point; value = first_dir } =
    m |> Utils.Matrix.find_point ~f:Direction.parse in
  take_until m first_point first_dir

let main rows =
  let m = rows |> Utils.Matrix.parse ~f:Fn.id in
  try
    m
      |> solve
      |> List.dedup_and_sort ~compare:(Utils.Tuple.compare_tuple_simple Int.compare)
      |> List.length
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Utils.Matrix.Could_not_find_point_in_matrix ->
    Stdio.print_endline "Couldn't find the starting point"
  | Unknown_character_found c ->
    Stdio.print_endline @@ Printf.sprintf "Found an unexisting character: %c" c 
