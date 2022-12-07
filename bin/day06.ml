open Base
open Stdio

exception Solve_error of string

let rec windows str len : char list list =
  if String.length str < len then []
  else
    String.to_list (String.sub str ~pos:0 ~len)
    :: windows (String.sub str ~pos:1 ~len:(String.length str - 1)) len

let read_file_to_single_string filename =
  In_channel.with_file ~binary:false filename ~f:In_channel.input_all

let solve str n =
  let wins = windows str n in
  match
    List.findi wins ~f:(fun _i lst ->
        not (List.contains_dup lst ~compare:Char.compare))
  with
  | Some (i, _) -> i + n
  | None -> raise (Solve_error "no solution found")

let () =
  let lines = String.split_lines (read_file_to_single_string "day06.txt") in
  List.iter lines ~f:(fun ln ->
      print_endline ("Part 1: " ^ Int.to_string (solve ln 4)));
  List.iter lines ~f:(fun ln ->
      print_endline ("Part 2: " ^ Int.to_string (solve ln 14)))
