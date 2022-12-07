open Base
open Stdio

exception Parse_error of string

let compartments line =
  let len = String.length line in
  assert (len % 2 = 0);
  ( String.sub line ~pos:0 ~len:(len / 2),
    String.sub line ~pos:(len / 2) ~len:(len / 2) )

let priority c =
  match c with
  | c when Char.between c ~low:'a' ~high:'z' ->
      Char.to_int c - Char.to_int 'a' + 1
  | c when Char.between c ~low:'A' ~high:'Z' ->
      Char.to_int c - Char.to_int 'A' + 27
  | c -> raise (Parse_error ("No priority for: " ^ Char.escaped c))

let in_all lst =
  let sets =
    List.map lst ~f:(fun str -> Set.of_list (module Char) (String.to_list str))
  in
  match sets with
  | [] -> None
  | s :: ss ->
      let shared = List.fold_left ss ~init:s ~f:Set.inter in
      List.hd (Set.to_list shared)

let sumi (lst : int list) = List.fold_left lst ~init:0 ~f:Int.( + )

let () =
  let lines = Aoc.Input.read_input_day_as_lines 3 in
  let prios =
    List.map lines ~f:(fun ln ->
        let a, b = compartments ln in
        priority (Option.value_exn (in_all [ a; b ])))
  in
  print_endline ("Part 1: " ^ Int.to_string (sumi prios));
  let prios2 =
    List.map (List.chunks_of lines ~length:3) ~f:(fun group ->
        priority (Option.value_exn (in_all group)))
  in
  print_endline ("Part 2: " ^ Int.to_string (sumi prios2))
