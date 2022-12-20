open Base
open Stdio
include Option.Let_syntax

exception Parse_error of string

type valve = { name : string; rate : int; tunnels_to : string list }
[@@deriving sexp]

let parse_line ln : valve =
  match
    Aoc.Input.split_by ln ~keep:(fun c ->
        not (List.mem [ ' '; '='; ';'; ',' ] c ~equal:Char.equal))
  with
  | "Valve" :: name :: _has :: _flow :: _rate :: rate_str :: _tunnels :: _leads
    :: _to :: _valves :: tunnels_to ->
      { name; rate = Int.of_string rate_str; tunnels_to }
  | _ -> raise (Parse_error "could not parse line")

module SrcDst = struct
  type t = { src : string; dst : string } [@@deriving sexp, hash, compare]
end

let floyd_warshall nodes =
  let all_nodes =
    List.dedup_and_sort
      (List.concat_map nodes ~f:(fun (src, dst, _) -> [ src; dst ]))
      ~compare:String.compare
  in

  let htl = Hashtbl.create (module SrcDst) in
  List.iter nodes ~f:(fun (src, dst, cost) ->
      Hashtbl.set htl ~key:{ src; dst } ~data:cost);

  List.iter all_nodes ~f:(fun via ->
      List.iter all_nodes ~f:(fun src ->
          List.iter all_nodes ~f:(fun dst ->
              Hashtbl.change htl { src; dst } ~f:(fun old ->
                  let costVia =
                    let ( let* ) = Option.Let_syntax.( >>= ) in
                    let* via1 = Hashtbl.find htl { src; dst = via } in
                    let* via2 = Hashtbl.find htl { src = via; dst } in
                    Option.return (via1 + via2)
                  in
                  Option.merge old costVia ~f:Int.min))));
  htl

type state = {
  name : string;
  time_left : int;
  flow_score : int;
  visited : int;
  path : string list;
}
[@@deriving sexp]

let compare_state a b = compare a.flow_score b.flow_score

let rec visit ~next state =
  Option.value ~default:state
    (List.max_elt
       (List.map (next state) ~f:(visit ~next))
       ~compare:compare_state)

let () =
  let valves = Aoc.Input.get_input_parsed 16 ~parser:parse_line in
  let table =
    List.concat_map valves ~f:(fun valve ->
        List.map valve.tunnels_to ~f:(fun dst -> (valve.name, dst, 1)))
  in
  let resolved = floyd_warshall table in
  let all_valid_rates =
    valves
    |> List.filter ~f:(fun v -> v.rate > 0)
    |> List.map ~f:(fun valve -> (valve.name, valve.rate))
  in
  let find_time_src_dst src dst = Hashtbl.find_exn resolved { src; dst } in
  let all_state_bitmaps =
    all_valid_rates
    |> List.mapi ~f:(fun i (name, _) -> (name, Int.shift_left 1 i))
    |> Hashtbl.of_alist_exn (module String)
  in
  let find_visited_bitmap = Hashtbl.find_exn all_state_bitmaps in
  let max_score = ref 0 in
  let next ht s =
    List.filter_map all_valid_rates ~f:(fun (dst, flow_rate) ->
        max_score := max !max_score s.flow_score;
        Hashtbl.update ht s.visited ~f:(function
          | None -> s.flow_score
          | Some old -> max old s.flow_score);
        let new_time_left = s.time_left - find_time_src_dst s.name dst - 1 in
        let visited = find_visited_bitmap dst in
        let new_visited = Int.bit_or s.visited visited in
        if new_time_left <= 0 || Int.bit_and s.visited visited > 0 then None
        else
          Some
            {
              name = dst;
              time_left = new_time_left;
              flow_score = s.flow_score + (flow_rate * new_time_left);
              visited = new_visited;
              path = dst :: s.path;
            })
  in
  let part1_ht = Hashtbl.create (module Int) in
  let part2_ht = Hashtbl.create (module Int) in
  let _ =
    visit ~next:(next part1_ht)
      { name = "AA"; time_left = 30; flow_score = 0; visited = 0; path = [] }
  in
  printf "Part 1: %d\n" !max_score;
  let _ =
    visit ~next:(next part2_ht)
      { name = "AA"; time_left = 26; flow_score = 0; visited = 0; path = [] }
  in
  let max_with_elephant =
    (* Is there any nicer way to do this which is also fast? *)
    let open Bigarray in
    let source = Hashtbl.to_alist part2_ht |> Array.of_list in
    let visited =
      Array1.of_array Bigarray.int Bigarray.c_layout (Array.map source ~f:fst)
    in
    let flow =
      Array1.of_array Bigarray.int Bigarray.c_layout (Array.map source ~f:snd)
    in

    let max_flow = ref 0 in
    for i = 0 to Array.length source - 1 do
      for j = 0 to Array.length source - 1 do
        if Int.( land ) (Array1.get visited i) (Array1.get visited j) = 0 then
          let new_flow = Array1.get flow i + Array1.get flow j in
          if new_flow > !max_flow then max_flow := new_flow
      done
    done;
    !max_flow
  in
  printf "Part 2: %d\n" max_with_elephant
