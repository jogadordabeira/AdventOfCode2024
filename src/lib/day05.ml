open Base

let middle xs =
  let len = List.length xs in
  if len % 2 = 0 then None else Some (List.nth_exn xs (len / 2))

let parse_rule rule_raw =
  let regex = Re.compile @@ Re.Pcre.re "([0-9]+)\\|([0-9]+)" in
  try
    let r = Re.exec regex (String.strip rule_raw) in
    let x = Re.Group.get r 1 |> Int.of_string in
    let y = Re.Group.get r 2 |> Int.of_string in
    Some (x, y)
  with _ -> None

let parse_rules rules_raw =
  rules_raw
    |> List.filter_map ~f:parse_rule
    |> List.fold ~init:(Map.empty (module Int)) ~f:(fun acc (x, y) ->
        Map.update acc x ~f:(fun data ->
          let y_list = Set.singleton (module Int) y in
          match data with
          | None -> y_list
          | Some xs -> Set.union xs y_list))

let parse_series series_raw =
  series_raw
    |> List.map ~f:(fun x ->
        x
          |> String.split ~on:','
          |> List.filter_map ~f:Utils.Int.int_of_string_or_none)

let is_well_ordered rules serie =
  serie |> Utils.List.pair |> List.for_all ~f:(fun (x, y) -> 
    match Map.find rules x with
    | Some v when Set.mem v y -> true
    | _ -> false)

let solve rules_raw series_raw =
  let rules = parse_rules rules_raw in
  let series = parse_series series_raw in
  series
    |> List.filter_map ~f:(fun serie ->
        if is_well_ordered rules serie then middle serie else None)

let main rows =
  rows
    |> List.split_while ~f:(fun x -> not @@ String.is_empty x)
    |> (fun (bef, aft) -> solve bef (List.drop aft 1))
    |> List.fold ~init:0 ~f:(+)
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline
