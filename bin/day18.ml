open Base
open Stdio

exception Error of string

module Coord = struct
  type t = { x : int; y : int; z : int } [@@deriving hash, sexp, compare, equal]

  let of_string_exn str =
    match Aoc.Input.numbers str with
    | [ x; y; z ] -> { x; y; z }
    | _ -> raise (Error ("could not parse " ^ str))

  let down c = { x = c.x - 1; y = c.y - 1; z = c.z - 1 }

  let cross ({ x; y; z } as coord) =
    let all =
      [ { x = x - 1; y; z }; { x = x + 1; y; z } ]
      @ [ { x; y = y - 1; z }; { x; y = y + 1; z } ]
      @ [ { x; y; z = z - 1 }; { x; y; z = z + 1 } ]
    in
    List.filter all ~f:(Fn.compose not (equal coord))
end

let exposed_sides coords =
  let set = Hash_set.create (module Coord) in
  let rec go coords count =
    match coords with
    | [] -> (count, set)
    | c :: coords ->
        let cross = List.count (Coord.cross c) ~f:(Hash_set.mem set) in
        Hash_set.add set c;
        go coords (count + 6 - (2 * cross))
  in
  go coords 0

let crawler droplet queue =
  let seen = Hash_set.create (module Coord) in
  let has_seen c = Hash_set.mem seen c in
  let is_droplet c = Hash_set.mem droplet c in
  let rec go counter =
    match Queue.dequeue queue with
    | None -> counter
    | Some (_, s) when has_seen s -> go counter
    | Some (n, s) ->
        Hash_set.add seen s;
        let cross = Coord.cross s in
        let sides = List.count cross ~f:is_droplet in
        let new_n = if sides > 0 then 10 else n - 1 in
        let expand =
          cross
          |> List.filter ~f:(fun c -> (not (has_seen c)) && not (is_droplet c))
          |> List.map ~f:(fun c -> (new_n, c))
        in
        if new_n >= 0 then Queue.enqueue_all queue expand;
        go (sides + counter)
  in
  go 0

let () =
  let coords = Aoc.Input.get_input_parsed 18 ~parser:Coord.of_string_exn in
  let exposed, droplet = exposed_sides coords in
  printf "Part 1: %d\n" exposed;
  let start =
    Hash_set.min_elt droplet ~compare:Coord.compare |> function
    | None -> raise (Error "expected at least one coordinate")
    | Some c -> Coord.down c
  in
  let q = Queue.singleton (2, start) in
  let exposed2 = crawler droplet q in
  printf "Part 2: %d\n" exposed2