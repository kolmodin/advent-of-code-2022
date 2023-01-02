open Base
open Stdio

exception Parse_error of string

module Resource = struct
  type t = { ore : int; clay : int; obsidian : int; geode : int }
  [@@deriving sexp, compare]

  let empty = { ore = 0; clay = 0; obsidian = 0; geode = 0 }

  let add a b =
    {
      ore = a.ore + b.ore;
      clay = a.clay + b.clay;
      obsidian = a.obsidian + b.obsidian;
      geode = a.geode + b.geode;
    }

  let neg a =
    { ore = -a.ore; clay = -a.clay; obsidian = -a.obsidian; geode = -a.geode }

  let sub a b = add a (neg b)

  let mul a m =
    {
      ore = a.ore * m;
      clay = a.clay * m;
      obsidian = a.obsidian * m;
      geode = a.geode * m;
    }

  module Ops = struct
    let ( +: ) = add
    let ( *: ) = mul
    let ( -: ) = sub
  end
end

type blueprint = {
  num : int;
  purchase_options : (Resource.t * Resource.t) array;
}
[@@deriving sexp]

type state = { wallet : Resource.t; robots : Resource.t; time_left : int }
[@@deriving sexp]

let empty_state time_left =
  {
    wallet = Resource.empty;
    robots = { Resource.empty with ore = 1 };
    time_left;
  }

let parse_line ln =
  match Aoc.Input.numbers ln with
  | [
   bp_num;
   ore_ore;
   clay_ore;
   obsidian_ore;
   obsidian_clay;
   geode_ore;
   geode_obsidian;
  ] ->
      {
        num = bp_num;
        purchase_options =
          [|
            ( { Resource.empty with geode = 1 },
              { Resource.empty with ore = geode_ore; obsidian = geode_obsidian }
            );
            ( { Resource.empty with obsidian = 1 },
              { Resource.empty with ore = obsidian_ore; clay = obsidian_clay }
            );
            ( { Resource.empty with clay = 1 },
              { Resource.empty with ore = clay_ore } );
            ( { Resource.empty with ore = 1 },
              { Resource.empty with ore = ore_ore } );
          |];
      }
  | _ -> raise (Parse_error "could not parse line")

type recepit = MissingKind | MissingTime | Bought of state

let wait_and_purchase s buy_robot (cost : Resource.t) : recepit =
  if
    (cost.ore > 0 && s.robots.ore = 0)
    || (cost.clay > 0 && s.robots.clay = 0)
    || (cost.obsidian > 0 && s.robots.obsidian = 0)
    || (cost.geode > 0 && s.robots.geode = 0)
  then MissingKind
  else
    let ( // ) x y = (x + y - 1) / y in
    let wait cost wallet robot =
      if cost = 0 then 0 else max 1 (((cost - wallet) // robot) + 1)
    in
    let ore_wait = wait cost.ore s.wallet.ore s.robots.ore in
    let clay_wait = wait cost.clay s.wallet.clay s.robots.clay in
    let obsidian_wait =
      wait cost.obsidian s.wallet.obsidian s.robots.obsidian
    in
    let geode_wait = wait cost.geode s.wallet.geode s.robots.geode in
    let max_wait =
      max ore_wait (max clay_wait (max obsidian_wait geode_wait))
    in
    if s.time_left - max_wait < 1 then MissingTime
    else
      let open Resource.Ops in
      let new_wallet = s.wallet -: cost +: (s.robots *: max_wait) in
      let new_robots = s.robots +: buy_robot in
      Bought
        {
          wallet = new_wallet;
          robots = new_robots;
          time_left = s.time_left - max_wait;
        }

let rec max_geodes robots time_left =
  if time_left <= 0 then 0 else robots + max_geodes (robots + 1) (time_left - 1)

let dfs (bp : blueprint) (s : state) : int =
  let global_best = ref 0 in
  let record candidate =
    if candidate > !global_best then global_best := candidate
  in
  let rec go s =
    record s.wallet.geode;
    if s.time_left = 0 then ()
    else
      let max_geodes =
        s.wallet.geode
        + max_geodes s.robots.geode
            (s.time_left
            - (if s.robots.obsidian = 0 then 1 else 0)
            - if s.robots.clay = 0 then 1 else 0)
      in
      if max_geodes <= !global_best then ()
      else
        let jump_to_end = ref false in
        Array.iter bp.purchase_options ~f:(fun (robot, cost) ->
            match wait_and_purchase s robot cost with
            | MissingKind -> ()
            | MissingTime -> jump_to_end := true
            | Bought s' -> go s');
        if !jump_to_end then
          record (s.wallet.geode + (s.robots.geode * s.time_left))
  in

  go s;
  !global_best

let () =
  let blueprints = Aoc.Input.get_input_parsed 19 ~parser:parse_line in
  let quality_levels =
    List.map blueprints ~f:(fun bp -> bp.num * dfs bp (empty_state 24))
  in
  printf "Part 1: %d\n%!" (List.fold ~init:0 quality_levels ~f:Int.( + ));
  let geodes_3 =
    List.map (List.take blueprints 3) ~f:(fun bp -> dfs bp (empty_state 32))
  in
  printf "Part 2: %d\n" (List.fold ~init:1 geodes_3 ~f:Int.( * ))
