open Base
open Stdio

exception Parse_error of string

type range = { start : int; stop : int }

let parse_line str =
  match String.split_on_chars str ~on:[ ','; '-' ] with
  | [ a; b; c; d ] ->
      ( { start = Int.of_string a; stop = Int.of_string b },
        { start = Int.of_string c; stop = Int.of_string d } )
  | _ -> raise (Parse_error ("could not parse input: " ^ str))

let parse lines = List.map lines ~f:parse_line
let fully_contains ra rb = ra.start <= rb.start && ra.stop >= rb.stop
let either_fully_contains ra rb = fully_contains ra rb || fully_contains rb ra
let partially_overlaps ra rb = not (ra.stop < rb.start || rb.stop < ra.start)

let () =
  let elfs = parse (Aoc.Input.read_input_day_as_lines 4) in
  let count_fully_contains =
    List.count elfs ~f:(fun (ra, rb) -> either_fully_contains ra rb)
  in
  print_endline ("Part 1: " ^ Int.to_string count_fully_contains);
  let count_partially_overlaps =
    List.count elfs ~f:(fun (ra, rb) -> partially_overlaps ra rb)
  in
  print_endline ("Part 2: " ^ Int.to_string count_partially_overlaps)
