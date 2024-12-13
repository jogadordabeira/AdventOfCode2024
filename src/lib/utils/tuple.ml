open Base

let compare_tuple cmp1 cmp2 (x, y) (x', y') =
  let x_cmp = cmp1 x x' in
  if x_cmp <> 0 then x_cmp else cmp2 y y'

let compare_tuple_simple cmp = compare_tuple cmp cmp
let equal_tuple cmp1 cmp2 t1 t2 = compare_tuple cmp1 cmp2 t1 t2 = 0
let equal_tuple' cmp = equal_tuple cmp cmp

let show (x, y) =
  Printf.sprintf "(%d, %d)" x y

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
