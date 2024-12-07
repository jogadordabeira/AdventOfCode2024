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
    rows
      |> List.fold ~init:0 ~f:(fun acc s ->
          s
            |> parse_case
            |> fun (v, xs) -> if case_is_solvable v xs then acc + v else acc)
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Parse_error -> Stdio.print_endline "Couldn't properly parse the challenge"
