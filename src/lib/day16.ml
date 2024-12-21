open Base

module Cell = struct
  type t = Empty | Wall | Start | End
    [@@deriving eq]

  exception Parse_exception

  let parse x = match x with
  | '.' -> Empty
  | '#' -> Wall
  | 'S' -> Start
  | 'E' -> End
  | _ -> raise Parse_exception
end

(* A* algorithm to get the best path from the origin to the destination, returns the distance *)
let a_star_algorithm ~origin ~destination ~nodes =
  let rec helper acc =

  in helper []

let main rows =
  rows
    |> Utils.Matrix.parse ~f:Cell.parse
    |> (fun m -> 
        (* not the smartest way to get the points here (going through the same data multiple times)
          but not important to solve this challenge so I'll ignore it *)
        let origin = m
          |> Utils.Matrix.find_point ~f:(fun x -> Option.some_if (Cell.equal Start x) x)
          |> Utils.Matrix.get_c in
        let destination = m
          |> Utils.Matrix.find_point ~f:(fun x -> Option.some_if (Cell.equal End x) x)
          |> Utils.Matrix.get_c in
        let free_nodes = m
          |> Utils.Matrix.find_points ~f:(fun x -> Option.some_if (Cell.equal Empty x || Cell.equal End x) x)
          |> List.map ~f:Utils.Matrix.get_c in
        a_star_algorithm ~origin ~destination ~nodes:free_nodes)
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline
