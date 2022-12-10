open Base
open Stdio

exception Parse_error of string

type instr = Noop | Addx of int

let parse_line ln =
  match String.split_on_chars ln ~on:[ ' ' ] with
  | [ "noop" ] -> Noop
  | [ "addx"; i ] -> Addx (Int.of_string i)
  | _ -> raise (Parse_error ("could not parse: " ^ ln))

let rec exec instrs ~init:xval =
  match instrs with
  | [] -> []
  | x :: xs -> (
      match x with
      | Noop -> xval :: exec xs ~init:xval
      | Addx xdiff -> xval :: xval :: exec xs ~init:(xval + xdiff))

(* Part 1*)

let signal_strengths xvals =
  List.map2_exn xvals (List.range 1 (List.length xvals + 1)) ~f:Int.( * )

let sample_points = [ 20; 60; 100; 140; 180; 220 ]
let sumi lst = List.fold lst ~init:0 ~f:Int.( + )

(* Part 2 *)

let render_pixels xvals =
  let crt = List.range 0 240 in
  List.map2_exn xvals crt ~f:(fun sprite_pos pixel_pos ->
      if
        Int.between (pixel_pos % 40) ~low:(sprite_pos - 1) ~high:(sprite_pos + 1)
      then '#'
      else '.')

let render_as_lines pixels =
  pixels |> List.chunks_of ~length:40 |> List.map ~f:String.of_char_list

let () =
  let instrs = Aoc.Input.get_input_parsed 10 ~parser:parse_line in
  let xvals = instrs |> exec ~init:1 in
  let strengths = signal_strengths xvals in
  let sampled_strengths =
    List.map sample_points ~f:(fun s -> List.nth_exn strengths (s - 1))
  in
  print_endline ("Part 1: " ^ Int.to_string (sumi sampled_strengths));
  print_endline "Part 2:";
  List.iter (render_as_lines (render_pixels xvals)) ~f:print_endline
