open Base
open Stdio

exception Parse_error of string

type valve = { name : string; rate : int; tunnels_to : string list }
[@@deriving sexp]

let parse_line ln : valve =
  match
    Aoc.Input.split_by ln ~sep:(fun c ->
        List.mem [ ' '; '='; ';'; ',' ] c ~equal:Char.equal)
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

type state = { name : string; time_left : int; flow_score : int; visited : int }
[@@deriving sexp]

let () =
  let valves = Aoc.Input.get_input_parsed 16 ~parser:parse_line in
  let cost_table =
    List.concat_map valves ~f:(fun valve ->
        List.map valve.tunnels_to ~f:(fun dst -> (valve.name, dst, 1)))
  in
  let resolved = floyd_warshall cost_table in
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

  let rec traverse_all s ht =
    Hashtbl.update ht s.visited ~f:(function
      | None -> s.flow_score
      | Some old -> max old s.flow_score);
    List.iter all_valid_rates ~f:(fun (dst, flow_rate) ->
        let new_time_left = s.time_left - find_time_src_dst s.name dst - 1 in
        let visited = find_visited_bitmap dst in
        if new_time_left > 0 && Int.bit_and s.visited visited = 0 then
          Fn.ignore
          @@ traverse_all
               {
                 name = dst;
                 time_left = new_time_left;
                 flow_score = s.flow_score + (flow_rate * new_time_left);
                 visited = Int.bit_or s.visited visited;
               }
               ht);
    ht
  in

  let part1_ht =
    traverse_all
      { name = "AA"; time_left = 30; flow_score = 0; visited = 0 }
      (Hashtbl.create (module Int))
  in
  let part2_ht =
    traverse_all
      { name = "AA"; time_left = 26; flow_score = 0; visited = 0 }
      (Hashtbl.create (module Int))
  in
  let max_score =
    Hashtbl.to_alist part1_ht |> List.map ~f:snd |> List.reduce_exn ~f:Int.max
  in
  printf "Part 1: %d\n" max_score;
  let max_with_elephant =
    (* Is there any nicer way to do this which is also fast? *)
    let source = Hashtbl.to_alist part2_ht |> Array.of_list in
    let visited = Array.map source ~f:fst in
    let flow = Array.map source ~f:snd in

    let max_flow = ref 0 in
    for i = 0 to Array.length source - 1 do
      for j = 0 to Array.length source - 1 do
        if Int.( land ) (Array.get visited i) (Array.get visited j) = 0 then
          let new_flow = Array.get flow i + Array.get flow j in
          if new_flow > !max_flow then max_flow := new_flow
      done
    done;
    !max_flow
  in
  printf "Part 2: %d\n" max_with_elephant
