open Base
open Stdio

exception Solve_error of string

let rec windows str len : char list list =
  if String.length str < len then []
  else
    String.to_list (String.sub str ~pos:0 ~len)
    :: windows (String.sub str ~pos:1 ~len:(String.length str - 1)) len

let solve str n =
  let wins = windows str n in
  match
    List.findi wins ~f:(fun _i lst ->
        not (List.contains_dup lst ~compare:Char.compare))
  with
  | Some (i, _) -> i + n
  | None -> raise (Solve_error "no solution found")

let () =
  let lines = Aoc.Input.read_input_day_as_lines 6 in
  List.iter lines ~f:(fun ln ->
      print_endline ("Part 1: " ^ Int.to_string (solve ln 4)));
  List.iter lines ~f:(fun ln ->
      print_endline ("Part 2: " ^ Int.to_string (solve ln 14)))
