open Base

module ClawMachine = struct
  type t =
    { a_x : int (* claw x incrementation when pressing A *)
    ; a_y : int (* claw y incrementation when pressing A *)

    ; b_x : int (* claw x incrementation when pressing B *)
    ; b_y : int (* claw y incrementation when pressing B *)

    ; p_x : int (* prize x coordinate *)
    ; p_y : int (* prize y coordinate *)
    }

  exception Parse_exception

  let parse_button row =
    let regex = Re.compile
      @@ Re.Pcre.re "Button [A|B]: X\\+([0-9]+), Y\\+([0-9]+)" in
    try
      let r = Re.exec regex (String.strip row) in
      let x = Re.Group.get r 1 |> Int.of_string in
      let y = Re.Group.get r 2 |> Int.of_string in
      (x, y)
    with _ -> raise Parse_exception

  let parse_prize row =
    let regex = Re.compile
      @@ Re.Pcre.re "Prize: X=([0-9]+), Y=([0-9]+)" in
    try
      let r = Re.exec regex (String.strip row) in
      let x = Re.Group.get r 1 |> Int.of_string in
      let y = Re.Group.get r 2 |> Int.of_string in
      (x, y)
    with _ -> raise Parse_exception
  
  let parse a_row b_row prize_row =
    let (a_x, a_y) = parse_button a_row in
    let (b_x, b_y) = parse_button b_row in
    let (p_x, p_y) = parse_prize prize_row in
    { a_x; a_y; b_x; b_y; p_x; p_y }
  
  (* thanks Reddit for the Cramer's rule suggestion, very helpful
    c.f. https://en.wikipedia.org/wiki/Cramer%27s_rule *)
  let solve { a_x; a_y; b_x; b_y; p_x; p_y } =
    let a = ((p_x * b_y) - (p_y * b_x)) / ((a_x * b_y) - (a_y * b_x)) in
    let b = ((a_x * p_y) - (a_y * p_x)) / ((a_x * b_y) - (a_y * b_x)) in
    if ((a * a_x) + (b * b_x) = p_x) && ((a * a_y) + (b * b_y) = p_y) then 3 * a + b else 0
end

let parse rows =
  let rec helper acc rem_rows =
    if List.length rem_rows <= 2 then acc 
    else
      match rem_rows with
      | a_row::b_row::prize_rows::rem_rows' ->
        let acc' = (ClawMachine.parse a_row b_row prize_rows)::acc in
        helper acc' (List.drop rem_rows' 1)
      | _ -> acc
  in helper [] rows

let main rows =
  try
    let cms = parse rows in
    cms
      |> List.fold ~init:0 ~f:(fun acc cm -> acc + ClawMachine.solve cm)
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline;
    cms
      |> List.map ~f:(fun { ClawMachine.a_x; a_y; b_x; b_y; p_x; p_y } ->
          { ClawMachine.a_x; a_y; b_x; b_y; p_x = p_x + 10000000000000; p_y = p_y + 10000000000000 })
      |> List.fold ~init:0 ~f:(fun acc cm -> acc + ClawMachine.solve cm)
      |> Printf.sprintf "Part 2: %d"
      |> Stdio.print_endline
  with
  | ClawMachine.Parse_exception ->
      Stdio.prerr_endline "Could not parse one of the machines"
