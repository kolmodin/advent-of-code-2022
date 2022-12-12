open Base
open Stdio

type 'a t = { maxx : int; maxy : int; contents : 'a array }

let of_lines lns ~char_parser =
  let maxy = List.length lns in
  let maxx = String.length (Option.value ~default:"" (List.hd lns)) in
  List.iter lns ~f:(fun ln -> assert (String.length ln = maxx));
  let contents =
    Array.of_list
      (List.map (String.to_list (String.concat ~sep:"" lns)) ~f:char_parser)
  in
  { maxx; maxy; contents }

let coord_to_index board (coord : Coord.t) = (board.maxx * coord.y) + coord.x
let index_to_coord board idx = Coord.of_x_y (idx % board.maxx) (idx / board.maxx)
let get board coord = Array.get board.contents (coord_to_index board coord)

let set board coord value =
  Array.set board.contents (coord_to_index board coord) value

let find1 board ~f =
  match Array.findi board.contents ~f:(fun _ x -> f x) with
  | Some (idx, _) -> Some (index_to_coord board idx)
  | None -> None

let bounds board = (Coord.of_x_y 0 0, Coord.of_x_y board.maxx board.maxy)

let of_int_lines =
  of_lines ~char_parser:(fun c -> Int.of_string (Char.to_string c))

let of_char_lines = of_lines ~char_parser:(fun c -> c)

let to_string board =
  board.contents |> Array.to_list
  |> List.chunks_of ~length:board.maxx
  |> List.map ~f:String.of_char_list

let print board = List.iter ~f:print_endline (to_string board)
