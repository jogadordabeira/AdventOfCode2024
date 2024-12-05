open Base

let pair xs =
  match xs with
  | [] -> []
  | [_] -> []
  | _ -> List.zip_exn (List.drop_last_exn xs) (List.drop xs 1)

let%test "Pair (1)" =
  List.equal (Tuple.equal_tuple' Int.compare)
    [(1,2); (2,3)] @@ pair [1;2;3]

let%test "Pair (2)" =
  List.equal (Tuple.equal_tuple' Int.compare)
    [] @@ pair [1]

let%test "Pair (3)" =
  List.equal (Tuple.equal_tuple' Int.compare)
    [] @@ pair []

let%test "Pair (4)" =
  List.equal (Tuple.equal_tuple' Int.compare)
  [(1,2)] @@ pair [1;2]
