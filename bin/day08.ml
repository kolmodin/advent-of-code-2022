open Base
open Stdio
open Aoc

let board_all_pos (board : 'a Board.t) =
  let poss = ref [] in
  for y = 0 to board.maxy - 1 do
    for x = 0 to board.maxx - 1 do
      poss := Coord.of_x_y x y :: !poss
    done
  done;
  List.rev !poss

let rec steps_within_bounds bounds ~pos ~step =
  let pos' = Coord.add pos step in
  if Aoc.Coord.within_bounds bounds pos' then
    pos' :: steps_within_bounds bounds ~pos:pos' ~step
  else []

let board_visible board =
  let res = ref [] in

  let check highest pos =
    let here = Board.get board pos in
    if here > !highest then (
      highest := here;
      res := pos :: !res)
  in

  for y = 0 to board.maxy - 1 do
    let highest = ref (-1) in
    for x = 0 to board.maxx - 1 do
      check highest (Coord.of_x_y x y)
    done;

    highest := -1;

    for x = board.maxx - 1 downto 0 do
      check highest (Coord.of_x_y x y)
    done
  done;

  for x = 0 to board.maxx - 1 do
    let highest = ref (-1) in
    for y = 0 to board.maxy - 1 do
      check highest (Coord.of_x_y x y)
    done;

    highest := -1;

    for y = board.maxy - 1 downto 0 do
      check highest (Coord.of_x_y x y)
    done
  done;

  List.dedup_and_sort !res ~compare:Coord.compare

let rec see (home : int) (lst : int list) : int list =
  match lst with
  | [] -> []
  | x :: xs when x < home -> x :: see home xs
  | x :: _ -> [ x ]

let scienic_distance board ~pos ~dir =
  let steps = steps_within_bounds (Board.bounds board) ~pos ~step:dir in
  let heights = List.map steps ~f:(fun p -> Board.get board p) in
  List.length (see (Board.get board pos) heights)

let product list = List.fold list ~init:1 ~f:Int.( * )

let scenic_score board pos =
  let dirs = [ Coord.up; Coord.down; Coord.left; Coord.right ] in
  product (List.map dirs ~f:(fun dir -> scienic_distance board ~pos ~dir))

let best_scenic_score board =
  Option.value_exn
    (List.max_elt
       (List.map (board_all_pos board) ~f:(scenic_score board))
       ~compare:Int.compare)

let read_board () = Aoc.Input.get_input_board 8 ~char_parser:(fun c -> Int.of_string (Char.to_string c))

let () =
  let board = read_board () in
  let poss = board_visible board in
  print_endline ("Part 1: Found " ^ Int.to_string (List.length poss) ^ " trees");
  print_endline
    ("Part 2: Best scienic score: " ^ Int.to_string (best_scenic_score board))
