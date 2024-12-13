open Base
open Utils.Matrix
open Utils.Tuple

let compare_sides (c1, c2) (c3, c4) =
  let s1 = Set.of_list (module IntTuple) [c1; c2] in
  let s2 = Set.of_list (module IntTuple) [c3; c4] in
  Set.compare_direct s1 s2

let calculate_group xs =
  xs
    |> Set.fold
        ~init:[]
        ~f:(fun acc (x, y) ->
          List.rev_append acc
            [ ((x, y), (x, y + 1))
            ; ((x, y + 1), (x + 1, y + 1))
            ; ((x + 1, y + 1), (x + 1, y))
            ; ((x + 1, y), (x, y))
            ])
    |> List.sort_and_group ~compare:compare_sides
    |> List.filter ~f:(fun l -> List.length l = 1)
    |> (fun side_l ->
        let nb_elems = Set.length xs in
        let nb_sides = List.length side_l in
        nb_sides * nb_elems)

let calculate_groups xs =
  xs |> List.fold ~init:0 ~f:(fun acc s -> acc + calculate_group s)

let solve_group xs =
  let cs = xs
    |> List.map ~f:(fun { c; value=_ } -> c)
    |> Set.of_list (module IntTuple) in
  let rec helper acc rem_cs prev_group current_group =
    if Set.is_empty rem_cs then (current_group::acc)
    else if
      (not (Set.is_empty current_group)
      && not (Set.is_empty prev_group)
      && Set.equal current_group prev_group)
      then helper (current_group::acc) rem_cs (Set.empty (module IntTuple)) (Set.empty (module IntTuple))
    else
      let new_group_start =
        if Set.is_empty current_group
          then Set.singleton (module IntTuple) @@ Set.min_elt_exn rem_cs
          else current_group in
      let new_group =
        new_group_start
          |> Set.fold ~init:new_group_start ~f:(fun acc (x, y) ->
              Set.union acc @@
                Set.inter rem_cs @@
                  Set.of_list (module IntTuple)
                    [ (x, y - 1)
                    ; (x + 1, y)
                    ; (x, y + 1)
                    ; (x - 1, y)]) in
      let rem_cs' = Set.diff rem_cs new_group in
      helper acc rem_cs' current_group new_group
  in calculate_groups @@
    helper [] cs (Set.empty (module IntTuple)) (Set.empty (module IntTuple))

let solve ps =
  ps
    |> List.sort_and_group
        ~compare:(fun { c=_; value=v1 } { c=_; value=v2 }
          -> Char.compare v1 v2)
    |> List.fold ~init:0 ~f:(fun acc xs -> acc + solve_group xs)

let main rows =
  rows
    |> Utils.Matrix.parse ~f:Fn.id
    |> Utils.Matrix.find_points ~f:Option.some
    |> solve
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline
