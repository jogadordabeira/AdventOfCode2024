open Base
open Utils.Tuple

let parse_tuple (row: string) : (int * int) option =
  row
    |> String.split ~on:' '
    |> List.filter_map ~f:Utils.Int.int_of_string_or_none
    |> (fun xs -> match xs with
        | [x; y] -> Some (x, y)
        | _ -> None)

let main (rows: string list) =
  rows
    |> List.fold ~init:([], []) ~f:(fun (left, right) x -> let (a, b) = Option.value_exn @@ parse_tuple x in (a :: left, b :: right))
    |> (fun (left, right) -> (List.sort ~compare:Int.ascending left, List.sort ~compare:Int.ascending right))
    |> (fun (sorted_left, sorted_right) -> List.zip_exn sorted_left sorted_right)
    |> List.fold ~init:0 ~f:(fun acc (x, y) -> acc + Int.abs(x - y))
    |> Printf.sprintf "Part 1: %d"
    |> Stdio.print_endline

(* Some tests *)
let%test "Parse (1)" = Option.equal
  IntTuple.equal
    (parse_tuple "111 32") @@ Some (111, 32)

let%test "Parse (2)" = Option.equal
  IntTuple.equal
    (parse_tuple "1 0") @@ Some (1, 0)

let%test "Parse (3)" = Option.equal
  IntTuple.equal
    (parse_tuple "01 2") @@ Some (1, 2)

let%test "Parse (4)" = Option.equal
  IntTuple.equal
    (parse_tuple "11132") @@ None

let%test "Parse (5)" = Option.equal
  IntTuple.equal
    (parse_tuple "1f1 32") @@ None