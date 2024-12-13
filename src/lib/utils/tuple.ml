open Base

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
