open Base
open Stdio

exception Parse_error of string

type move_instruction = { count : int; src : int; dst : int }

let stack_names = List.range 1 10

let parse_stacks lines =
  let cols = List.range ~stride:4 1 34 in
  let stacks =
    List.map cols ~f:(fun x ->
        List.filter_map lines ~f:(fun ln ->
            match String.get ln x with
            | c when Char.is_uppercase c -> Some c
            | _ -> None))
  in
  Map.of_alist_exn (module Int) (List.zip_exn stack_names stacks)

let parse_move_line ln =
  match String.split_on_chars ln ~on:[ ' ' ] with
  | [ "move"; count_str; "from"; src_str; "to"; dst_str ] ->
      {
        count = Int.of_string count_str;
        src = Int.of_string src_str;
        dst = Int.of_string dst_str;
      }
  | _ -> raise (Parse_error ("could not parse move line: " ^ ln))

let move1 stacks n f =
  let stack = Map.find_exn stacks n in
  let value, new_stack = f stack in
  (Map.set stacks ~key:n ~data:new_stack, value)

let rec move_part1 stacks instr =
  if instr.count = 0 then stacks
  else
    let stacks', crate =
      move1 stacks instr.src (fun s -> (List.hd_exn s, List.drop s 1))
    in
    let stacks'', _ = move1 stacks' instr.dst (fun s -> ((), crate :: s)) in
    move_part1 stacks'' { instr with count = instr.count - 1 }

let move_part2 stacks instr =
  let stacks', crates =
    move1 stacks instr.src (fun s -> List.split_n s instr.count)
  in
  fst (move1 stacks' instr.dst (fun s -> ((), crates @ s)))

let print_part str instrs stacks mover =
  let part1_stack = List.fold instrs ~init:stacks ~f:mover in
  print_string str;
  Map.to_alist ~key_order:`Increasing part1_stack
  |> List.iter ~f:(fun (_, st) -> print_string (Char.escaped (List.hd_exn st)));
  print_endline ""

let () =
  let lines = Aoc.Input.read_input_day_as_lines 5 in
  let init_config_str =
    List.take_while lines ~f:(String.is_prefix ~prefix:"[")
  in
  let move_instrs =
    lines
    |> List.filter ~f:(String.is_prefix ~prefix:"move")
    |> List.map ~f:parse_move_line
  in
  let init_stacks = parse_stacks init_config_str in
  print_part "Part 1: " move_instrs init_stacks move_part1;
  print_part "Part 2: " move_instrs init_stacks move_part2
