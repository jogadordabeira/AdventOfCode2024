open Base

exception Could_not_find_point_in_matrix

type 'a point_info = 
  { c : (int * int)
  ; value : 'a
  }

let find_point (matrix : 'a array array) ~f : 'b point_info  =
  matrix
  |> Array.find_mapi ~f:(fun row_i column ->
         column
         |> Array.find_mapi ~f:(fun column_i p ->
                match f p with
                | None -> None
                | Some v -> Some (column_i, v))
         |> Option.bind ~f:(fun (column_i, p) -> Some (column_i, row_i, p)))
  |> function
  | None -> raise Could_not_find_point_in_matrix
  | Some (x, y, v) -> { c = (x, y); value = v }

let find_points (matrix : 'a array array) ~f : ('b point_info) list =
  matrix
  |> Array.concat_mapi ~f:(fun row_i column ->
      column
        |> Array.filter_mapi ~f:(fun column_i x ->
            match f x with
            | None -> None
            | Some v -> Some { c = (column_i, row_i); value = v }))
  |> Array.to_list

exception Could_not_get_all_coordinates_from_matrix

let all_coordinates matrix =
  try
    let y_max = Array.length matrix in
    let x_max = Array.length matrix.(0) in
    Array.cartesian_product
      (List.range 0 x_max ~start:`inclusive ~stop:`exclusive |> List.to_array)
      (List.range 0 y_max ~start:`inclusive ~stop:`exclusive |> List.to_array)
  with Invalid_argument _ -> raise Could_not_get_all_coordinates_from_matrix

let parse rows ~f =
  rows
    |> List.map ~f:(fun s -> s |> String.to_array |> Array.map ~f)
    |> List.to_array

let show m to_show =
  m |> Array.iter ~f:(fun row -> row |> Array.iter ~f:(fun x -> Stdio.printf "%c" @@ to_show x); Stdio.print_endline "")
