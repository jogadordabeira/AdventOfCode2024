open Base

module Robot = struct
  type t = 
    { p_x : int
    ; p_y : int
    ; v_x : int
    ; v_y : int
    }
  
  exception Parse_exception

  let parse row =
    let regex = Re.compile
      @@ Re.Pcre.re "p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)" in
    try
      let r = Re.exec regex (String.strip row) in
      let p_x = Re.Group.get r 1 |> Int.of_string in
      let p_y = Re.Group.get r 2 |> Int.of_string in
      let v_x = Re.Group.get r 3 |> Int.of_string in
      let v_y = Re.Group.get r 4 |> Int.of_string in
      { p_x; p_y; v_x; v_y }
    with _ -> raise Parse_exception
  
  let show { p_x; p_y; v_x; v_y } =
    Printf.sprintf "p=(%d, %d) v=(%d, %d)" p_x p_y v_x v_y

  let pos_after { p_x; p_y; v_x; v_y } ~after ~tiles_wide ~tiles_tall=
    let x = (p_x + (v_x * after)) % tiles_wide in
    let y = (p_y + (v_y * after)) % tiles_tall in
    (x, y)
end

let safety_factor ps ~tiles_wide ~tiles_tall =
  let mid_x = tiles_wide / 2 and mid_y = tiles_tall / 2 in
  let top_left = ps
    |> List.filter ~f:(fun (x, y) -> x < mid_x && y < mid_y)
    |> List.length in
  let top_right = ps
    |> List.filter ~f:(fun (x, y) -> x > mid_x && y < mid_y)
    |> List.length in
  let bottom_left = ps
    |> List.filter ~f:(fun (x, y) -> x < mid_x && y > mid_y)
    |> List.length in
  let bottom_right = ps
    |> List.filter ~f:(fun (x, y) -> x > mid_x && y > mid_y)
    |> List.length in
  top_left * top_right * bottom_left * bottom_right

let main rows =
  let tiles_wide = 101 and tiles_tall = 103 in
  try
    rows
      |> List.map ~f:(fun r ->
          r 
            |> Robot.parse
            |> Robot.pos_after ~after:100 ~tiles_wide ~tiles_tall)
      |> safety_factor ~tiles_wide ~tiles_tall
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Robot.Parse_exception ->
      Stdio.prerr_endline "Could not parse one of the given robots"
