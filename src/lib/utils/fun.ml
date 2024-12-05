open Base

let apply_n ~f ~x ~n =
  let rec helper acc current i =
    let current' = f current in
    let acc' = current'::acc in
    let i' = i + 1 in
    if i' >= n then List.rev acc' else helper acc' current' i'
  in helper [x] x 0

let%test "Apply N" =
  List.equal Int.equal
    [0; 1; 2; 3] @@ apply_n ~f:(fun x -> x+1) ~x:0 ~n:3
