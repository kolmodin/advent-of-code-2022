open Base
open Stdio

type board = { maxx : int; maxy : int; content : int array }
type pos = { x : int; y : int }

(*
   let board_bounds board =
     ({ x = 0; y = 0 }, { x = board.maxx - 1; y = board.maxy }) *)

let board_all_pos board =
  let poss = ref [] in
  for y = 0 to board.maxy - 1 do
    for x = 0 to board.maxx - 1 do
      poss := { x; y } :: !poss
    done
  done;
  List.rev !poss

let within_bounds board pos =
  pos.x >= 0 && pos.x < board.maxx && pos.y >= 0 && pos.y < board.maxy

let pos_add a b = { x = a.x + b.x; y = a.y + b.y }

let rec steps_within_bounds board ~pos ~step =
  let pos' = pos_add pos step in
  if within_bounds board pos' then
    pos' :: steps_within_bounds board ~pos:pos' ~step
  else []

let compare_pos a b =
  let cmpy = compare_int a.y b.y in
  if cmpy = 0 then compare_int a.x b.x else cmpy

(* let pos_to_string pos = Int.to_string pos.x ^ "," ^ Int.to_string pos.y *)

let board_of_lines lns =
  let maxy = List.length lns in
  let maxx = String.length (Option.value ~default:"" (List.hd lns)) in
  List.iter lns ~f:(fun ln -> assert (String.length ln = maxx));
  let content =
    Array.of_list
      (List.map
         (String.to_list (String.concat ~sep:"" lns))
         ~f:(fun c -> Int.of_string (Char.to_string c)))
  in
  { maxx; maxy; content }

let pos_to_index board pos = (board.maxx * pos.y) + pos.x
let board_get board pos = Array.get board.content (pos_to_index board pos)

(* let board_set board pos value =
  Array.set board.content (pos_to_index board pos) value

let print_board board =
  for y = 0 to board.maxy - 1 do
    for x = 0 to board.maxx - 1 do
      print_string (Int.to_string (board_get board { x; y }))
    done;
    print_endline ""
  done *)

let board_visible board =
  let res = ref [] in

  let check highest pos =
    let here = board_get board pos in
    if here > !highest then (
      highest := here;
      res := pos :: !res)
  in

  for y = 0 to board.maxy - 1 do
    let highest = ref (-1) in
    for x = 0 to board.maxx - 1 do
      check highest { x; y }
    done;

    highest := -1;

    for x = board.maxx - 1 downto 0 do
      check highest { x; y }
    done
  done;

  for x = 0 to board.maxx - 1 do
    let highest = ref (-1) in
    for y = 0 to board.maxy - 1 do
      check highest { x; y }
    done;

    highest := -1;

    for y = board.maxy - 1 downto 0 do
      check highest { x; y }
    done
  done;

  List.dedup_and_sort !res ~compare:compare_pos

let rec see (home : int) (lst : int list) : int list =
  match lst with
  | [] -> []
  | x :: xs when x < home -> x :: see home xs
  | x :: _ -> [ x ]

let scienic_distance board ~pos ~dir =
  let steps = steps_within_bounds board ~pos ~step:dir in
  let heights = List.map steps ~f:(fun p -> board_get board p) in
  List.length (see (board_get board pos) heights)

let product list = List.fold list ~init:1 ~f:Int.( * )

let scenic_score board pos =
  let dirs =
    [ { x = 1; y = 0 }; { x = -1; y = 0 }; { x = 0; y = 1 }; { x = 0; y = -1 } ]
  in
  product (List.map dirs ~f:(fun dir -> scienic_distance board ~pos ~dir))

let best_scenic_score board =
  Option.value_exn
    (List.max_elt
       (List.map (board_all_pos board) ~f:(scenic_score board))
       ~compare:Int.compare)

let read_board () =
  let lines = Aoc.Input.read_input_day_as_lines 8 in
  board_of_lines lines

let () =
  let board = read_board () in
  let poss = board_visible board in
  print_endline ("Part 1: Found " ^ Int.to_string (List.length poss) ^ " trees");
  print_endline
    ("Part 2: Best scienic score: " ^ Int.to_string (best_scenic_score board))
