open Base

module T = struct
  type t = { x : int; y : int } [@@deriving compare, sexp_of, hash]
end

include T
include Comparator.Make (T)

let to_string coord = Int.to_string coord.x ^ "," ^ Int.to_string coord.y
let of_x_y x y = { x; y }
let origo = of_x_y 0 0
let add a b = { x = a.x + b.x; y = a.y + b.y }
let sub a b = { x = a.x - b.x; y = a.y - b.y }
let abs a = { x = abs a.x; y = abs a.y }
let manhattan a b = Int.abs (a.x - b.x) + Int.abs (a.y - b.y)
let up = of_x_y 0 (-1)
let down = of_x_y 0 1
let left = of_x_y (-1) 0
let right = of_x_y 1 0
let negate coord = of_x_y (-coord.x) (-coord.y)
let scale coord n = of_x_y (coord.x * n) (coord.y * n)

let norm coord =
  let x = if coord.x = 0 then 0 else coord.x / Int.abs(coord.x) in
  let y = if coord.y = 0 then 0 else coord.y / Int.abs(coord.y) in
  of_x_y x y

let within_bounds (top_left, bottom_right) pos =
  pos.x >= top_left.x && pos.x < bottom_right.x && pos.y >= top_left.y
  && pos.y < bottom_right.y

let cross coord = List.map ~f:(add coord) [ up; right; down; left ]

let line a b =
  assert (a.x = b.x || a.y = b.y);
  let xs = List.range ~stop:`inclusive (min a.x b.x) (max a.x b.x) in
  let ys = List.range ~stop:`inclusive (min a.y b.y) (max a.y b.y) in
  List.cartesian_product xs ys |> List.map ~f:(fun (x, y) -> of_x_y x y)

let expand_to_bounds coord ~expand =
  ( add coord (of_x_y (-expand) (-expand)),
    add coord (of_x_y (expand + 1) (expand + 1)) )
