open Base

let parse rows =
  rows |> Utils.Matrix.parse ~f:Fn.id

let read m c ~translate ~n =
  Utils.Fun.apply_n ~f:translate ~x:c ~n:n
    |> List.filter_map ~f:(fun (x, y) ->
        try
          let a = m.(y).(x) in
          Some a
        with _ -> None)
    |> (fun l -> if List.length l = n+1 then Some l else None)

let%test "Read" =
  let m = [|[|'A'; 'B'; 'C'; 'D'|];[|'E'; 'F'; 'G'; 'H'|]|] in
  Option.equal (List.equal Char.equal) (Some ['A'; 'B'; 'C'; 'D']) @@ read m (0, 0) ~translate:(fun (x, y) -> (x + 1, y)) ~n:3

let is_xmas rs = List.equal Char.equal ['X';'M';'A';'S'] rs

let solve m =
  Utils.Matrix.all_coordinates m
    |> Array.map ~f:(fun c ->
        [ (fun (x, y) -> (x, y - 1))
        ; (fun (x, y) -> (x + 1, y - 1))
        ; (fun (x, y) -> (x + 1, y))
        ; (fun (x, y) -> (x + 1, y + 1))
        ; (fun (x, y) -> (x, y + 1))
        ; (fun (x, y) -> (x - 1, y + 1))
        ; (fun (x, y) -> (x - 1, y))
        ; (fun (x, y) -> (x - 1, y - 1))
        ]
          |> List.filter_map ~f:(fun dir -> read m c ~translate:dir ~n:3)
          |> List.count ~f:is_xmas
    ) |> Array.reduce_exn ~f:(+)

let main rows =
  rows
    |> parse
    |> solve
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline
