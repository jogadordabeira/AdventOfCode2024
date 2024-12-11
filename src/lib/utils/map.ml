open Base

let pretty_print m =
  m
    |> Map.iteri ~f:(fun ~key ~data ->
        Stdio.print_endline @@ Printf.sprintf "%d -> %d" key data)