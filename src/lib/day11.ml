open Base

let solve nb_blinks stones =
  let stones_map = stones
    |> List.sort_and_group ~compare:Int.compare
    |> List.map ~f:(fun l -> let h = List.hd_exn l in (h, List.length l))
    |> Map.of_alist_exn (module Int) in

  let rec helper stones_map blinked =
    if blinked = nb_blinks then stones_map
    else
      let update_stone n v_opt =
        match v_opt with
        | None -> n
        | Some x -> x + n
      in

      let get_next_stones ~key:stone ~data:nb new_map =
        if stone = 0 then Map.update new_map 1 ~f:(update_stone nb)
        else
          let string_stone = Int.to_string stone in
          let len_str_stone = String.length string_stone in
          if Int.rem len_str_stone 2 = 0
            then
              let half_size = len_str_stone / 2 in
              let left_stone =
                Int.of_string (string_stone |> String.sub ~pos:0 ~len:half_size) in
              let right_stone =
                Int.of_string (string_stone |> String.sub ~pos:half_size ~len:half_size) in
              Map.update new_map left_stone ~f:(update_stone nb)
                |> (fun new_map' -> Map.update new_map' right_stone ~f:(update_stone nb))
            else
              Map.update new_map (stone * 2024) ~f:(update_stone nb)
      in

      let stones_map' = stones_map
        |> Map.fold ~init:(Map.empty (module Int)) ~f:get_next_stones in

      helper stones_map' (blinked + 1)
    in helper stones_map 0

let main rows =
  let xs =
    rows
      |> List.hd_exn
      |> String.split ~on:' '
      |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
  xs
    |> solve 25
    |> Map.fold ~init:0 ~f:(fun ~key:_key ~data acc -> acc + data)
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline;
  xs
    |> solve 75
    |> Map.fold ~init:0 ~f:(fun ~key:_key ~data acc -> acc + data)
    |> Printf.sprintf "Part 2: %d"
    |> Stdio.print_endline
