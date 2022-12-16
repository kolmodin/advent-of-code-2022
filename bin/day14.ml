open Base
open Stdio
open Aoc

exception Parse_error of string

type cell = Rock | Sand
type falling_coord = Falling of Coord.t | Invalid | FallingAbyss of int
type resting_or_abyss = Resting of Coord.t | Abyss of int

let pouring_from = Coord.of_x_y 500 0

let parse_line ln : Coord.t list =
  String.split_on_chars ln ~on:[ ' ' ]
  |> List.filter ~f:(fun w -> not (String.equal w "->"))
  |> List.map ~f:(fun w ->
         match String.split_on_chars w ~on:[ ',' ] with
         | [ x_str; y_str ] ->
             Coord.of_x_y (Int.of_string x_str) (Int.of_string y_str)
         | _ -> raise (Parse_error ("could not parse line: " ^ ln)))

let rec line_to_rocks (line : Coord.t list) : Coord.t list =
  match line with
  | a :: b :: rest -> Coord.line a b @ line_to_rocks (b :: rest)
  | _ -> []

let build_cave (paths : Coord.t list list) =
  let all_rocks = List.concat_map paths ~f:line_to_rocks in
  Hash_set.of_list (module Coord) all_rocks

let build_lowest_rock (cave : Coord.t Hash_set.t) =
  Hash_set.to_list cave
  |> List.map ~f:(fun (c : Coord.t) -> c.y)
  |> List.fold ~init:0 ~f:max

let falls =
  [
    Coord.down;
    Coord.add Coord.down Coord.left;
    Coord.add Coord.down Coord.right;
  ]

let rec find_final_pos cave lowest_rock_y has_infinity_floor coord =
  let hits =
    List.map falls ~f:(fun diff ->
        let pos = Coord.add coord diff in
        let fell_into_abyss =
          (not has_infinity_floor) && lowest_rock_y < pos.y
        in
        let fell_onto_infinity_floor =
          has_infinity_floor && pos.y >= lowest_rock_y + 2
        in
        let hit_rock = Hash_set.mem cave pos in
        match (fell_into_abyss, fell_onto_infinity_floor || hit_rock) with
        | true, _ -> FallingAbyss pos.x
        | _, false -> Falling pos
        | _, true -> Invalid)
  in
  let rec find_progress hits =
    match hits with
    | FallingAbyss pos :: _ -> Abyss pos
    | Falling pos :: _ ->
        find_final_pos cave lowest_rock_y has_infinity_floor pos
    | Invalid :: rest -> find_progress rest
    | [] -> Resting coord
  in
  find_progress hits

let drop_sand cave lowest_rock_y has_infinity_floor =
  let coord =
    find_final_pos cave lowest_rock_y has_infinity_floor pouring_from
  in
  match coord with
  | Abyss _ -> true
  | Resting new_coord ->
      Hash_set.add cave new_coord;
      false

let rec count_until_stop cave lowest_rock_y ~has_infinity_floor n =
  if has_infinity_floor && Hash_set.mem cave pouring_from then n
  else if drop_sand cave lowest_rock_y has_infinity_floor then n
  else count_until_stop cave lowest_rock_y ~has_infinity_floor (n + 1)

let () =
  let paths = Input.get_input_parsed 14 ~parser:parse_line in
  let cave1 = build_cave paths in
  let cave2 = build_cave paths in
  let lowest_rock_y = build_lowest_rock cave1 in
  printf "Part 1: %d\n"
    (count_until_stop cave1 lowest_rock_y ~has_infinity_floor:false 0);
  printf "Part 2: %d\n"
    (count_until_stop cave2 lowest_rock_y ~has_infinity_floor:true 0)
