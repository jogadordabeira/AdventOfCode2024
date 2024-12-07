open Base

let case_is_solvable v xs : bool =
  let rec helper ys =
    match ys with
    | (x::_) when (x > v) -> false
    | [x] when x = v -> true
    | (x::y::ys') ->
        let mult = x * y in
        let add = x + y in
        helper (mult::ys') || helper (add::ys')
    | _ -> false
  in helper xs

let case_is_solvable_2 v xs : bool =
  let rec helper ys =
    match ys with
    | (x::_) when (x > v) -> false
    | [x] when x = v -> true
    | (x::y::ys') ->
        let mult = x * y in
        let add = x + y in
        let conc = Int.of_string (String.concat [Int.to_string x; Int.to_string y]) in
        helper (mult::ys') || helper (add::ys') || helper (conc::ys')
    | _ -> false
  in helper xs

exception Parse_error

let parse_case s : (int * int list) =
  match s |> String.split ~on:':' with
  | [bef; aft] -> (
      try
        let v = Int.of_string bef in
        let xs = aft |> String.split ~on:' ' |> List.filter_map ~f:Utils.Int.int_of_string_or_none in
        (v, xs)
      with _ -> raise Parse_error)
  | _ -> raise Parse_error

let main rows =
  try
    let parsed_rows = rows |> List.map ~f:(fun s -> parse_case s) in
    parsed_rows
      |> List.fold ~init:0 ~f:(fun acc (v, xs) ->
        if case_is_solvable v xs then acc + v else acc)
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline;
    parsed_rows
      |> List.fold ~init:0 ~f:(fun acc (v, xs) ->
        if case_is_solvable_2 v xs then acc + v else acc)
      |> Printf.sprintf "Part 2: %d"
      |> Stdio.print_endline
  with
  | Parse_error -> Stdio.print_endline "Couldn't properly parse the challenge"
