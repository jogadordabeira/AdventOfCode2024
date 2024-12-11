open Base
open Utils.Matrix

(* there must be a smarter way to do this without all of this but I couldn't find any *)
module IntTuple = struct
  module T = struct
    type t = int * int
    let compare x y = Core.Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
    let t_of_sexp tuple = Core.Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
    let sexp_of_t tuple = Core.Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  end
  include T
  include Comparable.Make(T)
end

let get_reachable_nines m z =
  let rec helper acc pos prev =
    let (x, y) = pos in
    try
      let v = m.(y).(x) in
      if v = prev + 1 then
        if v = 9
          then (pos :: acc)
          else
            List.concat
              [ helper acc (x, y - 1) v
              ; helper acc (x, y + 1) v
              ; helper acc (x - 1, y) v
              ; helper acc (x + 1, y) v
              ]
      else
        acc
    with
    | _ -> acc
  in helper [] z (-1)

let solve m =
  let keep_only_coord { c; value=_value } = c in

  let zs = m |> Utils.Matrix.find_points ~f:(fun x -> Option.some_if (x = 0) x)
    |> List.map ~f:keep_only_coord in

  let rec helper acc rem_zs =
    match rem_zs with
    | [] -> acc
    | z::rem_zs' ->
      let reached_ns = Set.of_list (module IntTuple) @@ get_reachable_nines m z in
      helper (acc + Set.length reached_ns) rem_zs'
  in helper 0 zs

let main rows =
  try
    rows
      |> Utils.Matrix.parse ~f:(fun c -> c |> Char.to_string |> Int.of_string)
      |> solve
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Failure _ -> Stdio.print_endline "Wrong character in the input file?"
