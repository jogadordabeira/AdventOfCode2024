open Base

module BlockType = struct
  type t = Space of int | File of (int * int)
end

module DiskMap = struct
  type t = BlockType.t Queue.t

  let get_ids xs =
    let rec helper acc i id_n xs =
      match xs with
      | [] -> acc
      | (y::ys) ->
        if Int.rem i 2 = 0
          then helper (BlockType.File (y, id_n) :: acc) (i + 1) (id_n + 1) ys
          else helper (BlockType.Space y :: acc) (i + 1) id_n ys
    in Queue.of_list @@ List.rev @@ helper [] 0 0 xs

  let of_string s: t =
    s |> String.to_list
      |> List.filter_map ~f:(fun c ->
          c |> String.of_char |> Utils.Int.int_of_string_or_none)
      |> get_ids

  (* returns the new queue and a list of tuple of (x, id) (like files) *)
  let fill_space n xs =
    let rec helper acc rem_n xs =
      if rem_n <= 0 || Queue.is_empty xs then (xs, acc)
      else
        let last_v = xs |> Queue.dequeue_back_exn in
        match last_v with
        | BlockType.Space _n -> helper acc rem_n xs
        | BlockType.File (x, id) ->
          if x = rem_n then (xs, (x, id)::acc)
          else if x > rem_n then
            (Queue.enqueue xs (File (x - rem_n, id));
            (xs, (rem_n, id)::acc))
          else (* x < rem_n *)
            helper ((x, id)::acc) (rem_n - x) xs
    in (fun (xs, acc) -> (xs, List.rev acc)) @@ helper [] n xs
  
  (* calculate the checksum of x being in disk from i (inclusive) to j (exclusive) *)
  let calc i j x =
    List.range i j ~start:`inclusive ~stop:`exclusive
      |> List.fold ~init:0 ~f:(+)
      |> (fun res -> res * x)

  let checksum dm =
    let rec helper acc i xs =
      if Queue.is_empty xs then acc else
      match Queue.dequeue_exn xs with 
      | BlockType.File (n, id) ->
          let current_checksum = calc i (i + n) id in
          helper (acc + current_checksum) (i + n) xs
      | BlockType.Space n ->
        let (xs', files) = fill_space n xs in
        let current_checksum =
          files
            |> List.fold ~init:(0, i) ~f:(fun (acc, k) (x, id) -> (acc + calc k (k + x) id, k + x))
            |> (fun (acc, _) -> acc) in
        helper (acc + current_checksum) (i + n) xs'
    in helper 0 0 dm
end

let%test "Full checksum test" =
  DiskMap.of_string "2333133121414131402" |> DiskMap.checksum = 1928
  (* "2333133121414131402"
    -> "00...111...2...333.44.5555.6666.777.888899"
    -> "0099811188827773336446555566.............." *)

let%test "Full checksum test (2)" =
  DiskMap.of_string "12345" |> DiskMap.checksum = 60
  (* "12345" -> "0..111....22222" -> "022111222" = 60 *)

let%test "Full checksum test (3)" =
  DiskMap.of_string "112" |> DiskMap.checksum = 3
  (* "112" -> "0.11" -> "011." = 3 *)

let%test "Full checksum test (4)" =
  DiskMap.of_string "111" |> DiskMap.checksum = 1
  (* "111" -> "0.1" -> "01." = 1 *)

let%test "Full checksum test (5)" =
  DiskMap.of_string "12121" |> DiskMap.checksum = 4
  (* "12121" -> "0..1..2" -> "021...." = 4 *)

let main rows =
  try
    rows
      |> List.hd_exn
      |> DiskMap.of_string
      |> DiskMap.checksum
      |> Printf.sprintf "Part 1: %d"
      |> Stdio.print_endline
  with
  | Failure s -> Stdio.prerr_endline s
