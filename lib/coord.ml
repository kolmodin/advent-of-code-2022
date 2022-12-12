open Base

module T = struct
  type t = { x : int; y : int }

  let compare a b =
    let cmpy = compare_int a.y b.y in
    if cmpy = 0 then compare_int a.x b.x else cmpy

  let sexp_of_t t : Sexp.t =
    List [ Atom (Int.to_string t.x); Atom (Int.to_string t.y) ]
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
let negate coord = { x = -coord.x; y = -coord.y }

let within_bounds (top_left, bottom_right) pos =
   pos.x >= top_left.x && pos.x < bottom_right.x && pos.y >= top_left.y
   && pos.y < bottom_right.y

let cross coord = List.map ~f:(add coord) [ up; right; down; left ]

(*
   let compare a b =
     let cmpy = compare_int a.y b.y in
     if cmpy = 0 then compare_int a.x b.x else cmpy *)

let expand_to_bounds coord ~expand =
  ( add coord (of_x_y (-expand) (-expand)),
    add coord (of_x_y (expand + 1) (expand + 1)) )
(* 
let within_bounds (top_left, bottom_right) coord =
  Int.between coord.x ~low:top_left.x ~high:bottom_right.x
  && Int.between coord.y ~low:top_left.y ~high:bottom_right.y *)
