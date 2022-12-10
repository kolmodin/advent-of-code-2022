open Base
open Stdio

let read_file_to_single_string filename =
  In_channel.with_file ~binary:false filename ~f:In_channel.input_all

let args_as_list () = List.drop (Array.to_list (Sys.get_argv ())) 1

let read_input_day day =
  match args_as_list () with
  | [] -> read_file_to_single_string (Printf.sprintf "inputs/day%02d.txt" day)
  | [ "-" ] -> In_channel.input_all In_channel.stdin
  | filename :: _ -> read_file_to_single_string filename

let read_input_day_as_lines day = String.split_lines (read_input_day day)

let get_input_parsed day ~parser =
  read_input_day day |> String.split_lines |> List.map ~f:parser
