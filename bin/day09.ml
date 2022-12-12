open Base
open Stdio
open Aoc

exception Parse_error of string

type dir = U | R | D | L
type instr = dir * int

let diff_of_dir dir =
  match dir with
  | U -> Coord.up
  | D -> Coord.down
  | R -> Coord.right
  | L -> Coord.left

let parse_line ln : instr =
  match String.split_on_chars ln ~on:[ ' ' ] with
  | [ "U"; n ] -> (U, Int.of_string n)
  | [ "R"; n ] -> (R, Int.of_string n)
  | [ "D"; n ] -> (D, Int.of_string n)
  | [ "L"; n ] -> (L, Int.of_string n)
  | _ -> raise (Parse_error ("Could not parse line: " ^ ln))

type rope = { head : Coord.t; tail : Coord.t list }

(* Return the new tail position *)
let step1 ~head tail =
  let touching_bound = Coord.expand_to_bounds head ~expand:1 in
  if Coord.within_bounds touching_bound tail then tail
  else
    (* step diagonal towards head one step x one step y *)
    let long_diag = Coord.sub head tail in
    let abs = Coord.abs long_diag in
    let diag =
      Coord.of_x_y
        (if abs.x = 0 then 0 else long_diag.x / abs.x)
        (if abs.y = 0 then 0 else long_diag.y / abs.y)
    in
    Coord.add tail diag

let step rope dir =
  let head = Coord.add rope.head (diff_of_dir dir) in
  let rec update_tails ~head tails =
    match tails with
    | [] -> []
    | tail :: tails' ->
        let tail' = step1 ~head tail in
        tail' :: update_tails ~head:tail' tails'
  in
  { head; tail = update_tails ~head rope.tail }

let rec step_instrs rope instrs =
  match instrs with
  | [] -> []
  | (dir, n) :: instrs' ->
      let rec step_instrs1 rope dir n =
        if n = 0 then step_instrs rope instrs'
        else rope :: step_instrs1 (step rope dir) dir (n - 1)
      in
      step_instrs1 rope dir n

let unique_last_tail instrs rope =
  let trace = step_instrs rope instrs in
  let tail_trace = List.map trace ~f:(fun rope -> List.last_exn rope.tail) in
  let unique = List.dedup_and_sort tail_trace ~compare:Coord.compare in
  List.length unique

let () =
  let instrs = Aoc.Input.get_input_parsed 9 ~parser:parse_line in
  let unique1 =
    unique_last_tail instrs { head = Coord.origo; tail = [ Coord.origo ] }
  in
  print_endline ("Part 1: " ^ Int.to_string unique1);
  let unique2 =
    unique_last_tail instrs
      { head = Coord.origo; tail = List.init 9 ~f:(fun _ -> Coord.origo) }
  in
  print_endline ("Part 2: " ^ Int.to_string unique2)