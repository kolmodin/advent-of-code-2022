open Base
open Stdio

exception Error of string

let rock_strs =
  [
    [ "..####." ];
    [ "...#..."; "..###.."; "...#..." ];
    [ "....#.."; "....#.."; "..###.." ];
    [ "..#...."; "..#...."; "..#...."; "..#...." ];
    [ "..##..."; "..##..." ];
  ]

type rock = int list
type tower = int list

let rock_str_rock rock : rock =
  List.map rock ~f:(fun ln ->
      List.foldi
        (String.to_list ln |> List.rev)
        ~init:0
        ~f:(fun i acc c ->
          Int.bit_or acc (if Char.equal c '#' then Int.shift_left 1 i else 0)))

type dir = Left | Right
type 'a repeater = { cycle : 'a array; count : int }

let make_jet str =
  {
    cycle =
      String.to_list str
      |> List.map ~f:(function '>' -> Right | _ -> Left)
      |> Array.of_list;
    count = 0;
  }

let make_rocks strs : rock repeater =
  { cycle = List.map strs ~f:rock_str_rock |> Array.of_list; count = 0 }

let next_repeated repeater =
  let n = repeater.count % Array.length repeater.cycle in
  (repeater.cycle.(n), { repeater with count = repeater.count + 1 })

let rec intersect rock tower =
  match (rock, tower) with
  | r :: rock, t :: tower -> r land t > 0 || intersect rock tower
  | [], _ -> false
  | _, [] -> true

let union rock tower =
  assert (not (intersect rock tower));
  let rec fold rock tower =
    match (rock, tower) with
    | r :: rock, t :: tower -> (r lor t) :: fold rock tower
    | [], tower -> tower
    | _ -> raise (Error "union")
  in
  fold rock tower

(* Shifts a rock left/right but respecting the tower width. *)
let shift_rock rock dir =
  let unmovable_cell = match dir with Right -> 1 | Left -> 64 in
  let has_unmovable_cell ln = ln land unmovable_cell > 0 in
  let shift i =
    match dir with Right -> Int.shift_right i 1 | Left -> Int.shift_left i 1
  in
  if List.exists rock ~f:has_unmovable_cell then rock
  else List.map rock ~f:shift

(* Shifting a rock left/right, respecting tower width and stopped rocks. *)
let move_dir rock tower dir =
  let shifted_rock = shift_rock rock dir in
  if intersect shifted_rock tower then rock else shifted_rock

let shave_tower tower =
  match tower with
  | t :: xs -> (t, xs)
  | _ -> raise (Error "expected tower to have at least height 1")

let rec repeat x n = match n with 0 -> [] | n -> x :: repeat x (n - 1)

(* Move one step to the side, then down. Repeat until stopped, integrate with tower. *)
let step_until_rest (rock : rock) (tower : tower) (jets : dir repeater) :
    tower * dir repeater =
  let rec go rock tower rev_tower jets =
    assert (not (intersect rock tower));
    let dir, jets = next_repeated jets in
    let rock = move_dir rock tower dir in
    assert (not (intersect rock tower));
    let level, tiny_tower = shave_tower tower in
    if intersect rock tiny_tower then
      let new_tower = List.rev rev_tower @ tower in
      let new_rock = repeat 0 (List.length rev_tower) @ rock in
      (union new_rock new_tower, jets)
    else go rock tiny_tower (level :: rev_tower) jets
  in
  go rock tower [] jets

(* Adds/removes empty rows so that there's room for the rock and 3 empty rows *)
let tower_for_rock (rock : rock) (tower : tower) : tower =
  let rock_len = List.length rock in
  let got = List.length (List.take_while tower ~f:(Int.equal 0)) in
  let expected = rock_len + 3 in
  if expected - got > 0 then repeat 0 (expected - got) @ tower
  else List.drop tower (got - expected)

let tower_height (tower : tower) =
  List.length (List.drop_while tower ~f:(Int.equal 0))

let hash_int_list lst =
  let s = Hash.alloc () |> Hash.reset ~seed:0 in
  List.hash_fold_t Int.hash_fold_t s lst |> Hash.get_hash_value

module Seen = struct
  type t = { jet_index : int; rock_index : int; tower_hash : int }
  [@@deriving hash, sexp, compare]
end

let step_n_rocks_fast rocks tower jets target_n =
  let seen = Hashtbl.create (module Seen) in
  let rec go rocks tower jets added_levels =
    if rocks.count = target_n then tower_height tower + added_levels
    else
      let rock, rocks = next_repeated rocks in
      let tower, jets = step_until_rest rock (tower_for_rock rock tower) jets in
      let state =
        Seen.
          {
            jet_index = jets.count % Array.length jets.cycle;
            rock_index = rocks.count % Array.length rocks.cycle;
            tower_hash = hash_int_list (List.take tower 30);
          }
      in
      match Hashtbl.find seen state with
      | None ->
          Hashtbl.change seen state ~f:(fun _ ->
              Some (rocks.count, tower_height tower));
          go rocks tower jets added_levels
      | Some (old_rock_count, old_tower_height) ->
          let delta_rocks = rocks.count - old_rock_count in
          let delta_height = tower_height tower - old_tower_height in
          let full_cycles = (target_n - rocks.count) / delta_rocks in
          let extra_added_rocks = delta_rocks * full_cycles in
          let extra_added_levels = delta_height * full_cycles in
          go
            { rocks with count = rocks.count + extra_added_rocks }
            tower jets
            (added_levels + extra_added_levels)
  in
  go rocks tower jets 0

let () =
  let jet_str = List.hd_exn (Aoc.Input.read_input_day_as_lines 17) in
  let jets = make_jet jet_str in
  let rocks = make_rocks rock_strs in
  printf "Part 1: %d\n" (step_n_rocks_fast rocks [] jets 2022);
  printf "Part 2: %d\n" (step_n_rocks_fast rocks [] jets 1_000_000_000_000)
