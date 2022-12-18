open Base
open Stdio
open Aoc

exception Parse_error of string

type interval = { left : int; right : int } [@@deriving sexp, compare]

let numbers ln =
  let is_num c = Char.is_digit c || Char.equal c '-' in
  let rec strip xs = take (List.drop_while xs ~f:(Fn.compose not is_num))
  and take xs =
    match xs with
    | [] -> []
    | xs ->
        let prefix, rest = List.split_while xs ~f:is_num in
        let num = Int.of_string (String.of_char_list prefix) in
        num :: strip rest
  in
  strip (String.to_list ln)

let parse_line ln =
  match numbers ln with
  | [ a; b; c; d ] -> (Coord.of_x_y a b, Coord.of_x_y c d)
  | _ -> raise (Parse_error "wrong number of numbers")

(* Part 1 *)

let calc_sensors_dist = List.map ~f:(fun (s, b) -> (s, Coord.manhattan s b))

let sensor_intersect (sensor : Coord.t) dist y =
  let see_low = sensor.y + dist in
  let see_high = sensor.y - dist in
  let dist_to_y = Int.abs (sensor.y - y) in
  if not (Int.between y ~low:see_high ~high:see_low) then None
  else
    let left = sensor.x - (dist - dist_to_y) in
    let right = sensor.x + (dist - dist_to_y) in
    Some { left; right }

let rec cat_options xs =
  match xs with
  | [] -> []
  | None :: xs -> cat_options xs
  | Some x :: xs -> x :: cat_options xs

let merge_intervals xs =
  let rec merge left right xs =
    match xs with
    | [] -> [ { left; right } ]
    | x :: xs when right < x.left -> { left; right } :: merge x.left x.right xs
    | x :: xs -> merge left (max right x.right) xs
  in
  match List.sort xs ~compare:compare_interval with
  | [] -> []
  | x :: xs -> merge x.left x.right xs

let count_positions intervals (beacons : Coord.t list) line =
  let interval_count =
    List.map intervals ~f:(fun i -> i.right - i.left + 1)
    |> List.fold ~init:0 ~f:Int.( + )
  in
  let beacons_within_intervals =
    List.cartesian_product beacons intervals
    |> List.map ~f:(fun (b, i) ->
           if line = b.y && Int.between b.x ~low:i.left ~high:i.right then 1
           else 0)
    |> List.fold ~init:0 ~f:Int.( + )
  in
  interval_count - beacons_within_intervals

(* Part 2 *)

let tuning_frequency (beacon : Coord.t) = (beacon.x * 4000000) + beacon.y

let is_outside_sensor sensor distance coord =
  Coord.manhattan sensor coord > distance

let is_outside_all_sensors sensors_with_dist coord =
  let hit =
    List.for_all sensors_with_dist ~f:(fun (s, d) ->
        is_outside_sensor s d coord)
  in
  hit

let diag_line start stop =
  let dir = Coord.norm (Coord.sub stop start) in
  assert (Int.abs (start.x - stop.x) = Int.abs (start.y - stop.y));
  let rec go acc =
    if Coord.compare acc stop = 0 then [ acc ]
    else acc :: go (Coord.add acc dir)
  in
  go start

let search_missing_beacon sensors_and_dist =
  let is_within_legal_bound (coord : Coord.t) =
    Int.between coord.x ~low:0 ~high:4000000
    && Int.between coord.y ~low:0 ~high:4000000
  in
  let search_around_sensor (sensor, dist) =
    let nearby_sensors_and_dist =
      List.sort sensors_and_dist ~compare:(fun (s1, _) (s2, _) ->
          compare (Coord.manhattan sensor s1) (Coord.manhattan sensor s2))
    in
    let test_diag_line points =
      List.find (points ()) ~f:(fun p ->
          is_within_legal_bound p
          && is_outside_all_sensors nearby_sensors_and_dist p)
    in
    let edge dir = Coord.add sensor (Coord.scale dir (dist + 1)) in
    let up = edge Coord.up in
    let right = edge Coord.right in
    let down = edge Coord.down in
    let left = edge Coord.left in
    let diag_lines =
      [
        (fun () -> diag_line up right);
        (fun () -> diag_line right down);
        (fun () -> diag_line down left);
        (fun () -> diag_line left up);
      ]
    in
    List.find_map diag_lines ~f:test_diag_line
  in
  List.find_map sensors_and_dist ~f:search_around_sensor

let () =
  let sensors_beacons = Input.get_input_parsed 15 ~parser:parse_line in
  let beacons =
    List.map sensors_beacons ~f:snd
    |> List.dedup_and_sort ~compare:Coord.compare
  in
  let sad = calc_sensors_dist sensors_beacons in
  let line_of_interest = 2000000 in
  let si =
    List.map sad ~f:(fun (s, d) -> sensor_intersect s d line_of_interest)
  in
  let merged = merge_intervals (cat_options si) in

  printf "Part 1: %d" (count_positions merged beacons line_of_interest);
  (*flush?*)
  print_endline "";

  let part2 = search_missing_beacon sad in
  match part2 with
  | Some hit ->
      printf
        !"Part 2: %{sexp:Coord.t} tuning freq=%d\n"
        hit (tuning_frequency hit)
  | _ -> printf "fail\n"
