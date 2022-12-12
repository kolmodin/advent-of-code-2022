open Base
open Stdio
open Aoc

type state = { coord : Coord.t; step_count : int; path : Coord.t list }

let () =
  let board = Input.get_input_board 12 ~char_parser:(fun c -> c) in
  let bounds = Board.bounds board in
  let initS =
    {
      coord =
        Option.value_exn (Board.find1 board ~f:(fun c -> Char.( = ) c 'S'));
      step_count = 0;
      path = [];
    }
  in
  let final_coord =
    Option.value_exn (Board.find1 board ~f:(fun c -> Char.( = ) c 'E'))
  in

  let height_of c =
    let offset = Char.to_int 'a' in
    -offset
    +
    match c with
    | 'S' -> Char.to_int 'a'
    | 'E' -> Char.to_int 'z'
    | _ -> Char.to_int c
  in

  let part1_final_state =
    Option.value_exn
      (Search.bfs
         (module Coord)
         initS
         (fun (s : state) -> s.coord)
         (fun from ->
           Coord.cross from.coord
           |> List.filter ~f:(Coord.within_bounds bounds)
           |> List.filter ~f:(fun coord ->
                  height_of (Board.get board coord)
                  <= 1 + height_of (Board.get board from.coord))
           |> List.map ~f:(fun coord ->
                  {
                    coord;
                    step_count = from.step_count + 1;
                    path = from.coord :: from.path;
                  }))
         (fun s -> Coord.compare s.coord final_coord = 0))
  in
  print_endline (Int.to_string part1_final_state.step_count);

  let initS2 = { coord = part1_final_state.coord; step_count = 0; path = [] } in
  let part2_final_state =
    Option.value_exn
      (Search.bfs
         (module Coord)
         initS2
         (fun (s : state) -> s.coord)
         (fun from ->
           Coord.cross from.coord
           |> List.filter ~f:(Coord.within_bounds bounds)
           |> List.filter ~f:(fun coord ->
                  height_of (Board.get board from.coord)
                  <= 1 + height_of (Board.get board coord))
           |> List.map ~f:(fun coord ->
                  {
                    coord;
                    step_count = from.step_count + 1;
                    path = from.coord :: from.path;
                  }))
         (fun s -> Char.( = ) (Board.get board s.coord) 'a'))
  in
  print_endline (Int.to_string part2_final_state.step_count)
