open Base
open Utils.Matrix

module WarehouseBlock = struct
  type t = Wall | Empty | Box | Robot
    [@@deriving eq]

  exception Parse_exception

  let parse c = match c with
  | '#' -> Wall
  | 'O' -> Box
  | '@' -> Robot
  | '.' -> Empty
  | _ -> raise Parse_exception

  let show b = match b with
  | Wall -> '#'
  | Box -> 'O'
  | Robot -> '@'
  | Empty -> '.'

  let gps_coordinate (x, y) = (100 * y) + x
end

module Warehouse = struct
  type t = WarehouseBlock.t array array

  let parse rows =
    rows |> Utils.Matrix.parse ~f:WarehouseBlock.parse
  
  let total_gps_coordinate w =
    w
      |> Utils.Matrix.find_points ~f:(fun x -> Option.some_if (WarehouseBlock.equal x Box) x)
      |> List.fold ~init:0 ~f:(fun acc { c; value=_ } -> acc + WarehouseBlock.gps_coordinate c)
end

module Command = struct
  type t = Up | Right | Down | Left
    [@@deriving show]

  exception Parse_exception

  let parse c = match c with
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _   -> raise Parse_exception

  let next (x, y) c = match c with
  | Up -> (x, y - 1)
  | Right -> (x + 1, y)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
end

module Context = struct
  type t =
    { w : Warehouse.t
    ; cmds : Command.t list
    ; pos : Utils.Tuple.IntTuple.t
    }

  let parse rows =
    let (bef, aft) = rows
      |> List.split_while ~f:(fun x -> not @@ String.is_empty x) in
    let w = Warehouse.parse bef in
    let pos = w
      |> Utils.Matrix.find_point
          ~f:(fun x -> Option.some_if (WarehouseBlock.equal x Robot) x)
      |> (fun { c; value=_value } -> c) in
    let cmds =
      List.drop aft 1
        |> String.concat
        |> String.to_list
        |> List.map ~f:Command.parse in
    { w; cmds; pos }

  let go_while (w: Warehouse.t) pos c: WarehouseBlock.t point_info list =
    let rec helper acc current_pos =
      let (x, y) = current_pos in
      let next_pos = Command.next current_pos c in
      let e = w.(y).(x) in
      match e with
      | Wall -> [] (* we hit a wall without finding an empty spot: nothing changes *)
      | Empty -> (* we successfully found an empty spot, we stop and return the acc *)
        { c=current_pos; value=e }::acc
      | Box | Robot -> (* we reached something that is possible to move, we continue *)
        helper ({ c=current_pos; value=e }::acc) next_pos
    in helper [] pos

  let move w pos c =
    let lps = go_while w pos c in
    match lps with
    | ({ c=_c; value }::_) when (WarehouseBlock.equal value Empty) ->
      let (cs, vs) = lps |> List.fold ~init:([], []) ~f:(fun (acc_cs, acc_vs) { c; value } -> (c::acc_cs, value::acc_vs)) in
      List.zip_exn cs (WarehouseBlock.Empty :: (List.drop_last_exn vs))
        |> List.iter ~f:(fun ((x, y), v) -> w.(y).(x) <- v;);
      w
    | _ -> w

  let move_all { w; cmds; pos } =
    cmds
      |> List.fold ~init:(w, pos) ~f:(fun (current_w, current_pos) cmd ->
          let new_w = move current_w current_pos cmd in
          let { c=new_pos; value=_ } = w |> Utils.Matrix.find_point ~f:(fun x -> Option.some_if (WarehouseBlock.equal x Robot) x) in
          (new_w, new_pos))
      |> Core.Tuple2.get1;
end

let main rows =
  try
    rows
      |> Context.parse
      |> Context.move_all
      |> Warehouse.total_gps_coordinate
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Command.Parse_exception ->
      Stdio.prerr_endline "Could not parse one of the commands."
  | WarehouseBlock.Parse_exception ->
      Stdio.prerr_endline "Could not parse on the warehouse blocks."
