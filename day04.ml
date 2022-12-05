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

let parse str = List.map (String.split_lines str) ~f:parse_line

let fully_contains ra rb = ra.start <= rb.start && ra.stop >= rb.stop
let either_fully_contains ra rb = fully_contains ra rb || fully_contains rb ra
let partially_overlaps ra rb = not (ra.stop < rb.start || rb.stop < ra.start)

let read_file_to_single_string filename =
  In_channel.with_file ~binary:false filename ~f:In_channel.input_all

let () =
  let elfs = parse (read_file_to_single_string "day04.txt") in
  let count_fully_contains =
    List.count elfs ~f:(fun (ra, rb) -> either_fully_contains ra rb)
  in
  print_endline ("Part 1: " ^ Int.to_string count_fully_contains);
  let count_partially_overlaps =
    List.count elfs ~f:(fun (ra, rb) -> partially_overlaps ra rb)
  in
  print_endline ("Part 2: " ^ Int.to_string count_partially_overlaps)
