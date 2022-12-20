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

let get_input_board day ~char_parser =
  read_input_day day |> String.split_lines |> Board.of_lines ~char_parser

let split_by ln ~keep =
  let rec strip xs = take (List.drop_while xs ~f:(Fn.compose not keep))
  and take xs =
    match xs with
    | [] -> []
    | xs ->
        let prefix, rest = List.split_while xs ~f:keep in
        String.of_char_list prefix :: strip rest
  in
  strip (String.to_list ln)

let numbers ln =
  split_by ln ~keep:(fun c -> Char.is_digit c || Char.equal c '-')
  |> List.map ~f:Int.of_string
