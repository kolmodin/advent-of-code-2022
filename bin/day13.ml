open Base
open Stdio
open Aoc

exception Parse_error of string

type packet = Lst of packet list | Num of int

let rec compare_packets (a : packet) (b : packet) : int =
  match (a, b) with
  | Num a, Num b -> compare a b
  | Lst _, Num _ -> compare_packets a (Lst [ b ])
  | Num _, Lst _ -> compare_packets (Lst [ a ]) b
  | Lst [], Lst (_ :: _) -> -1
  | Lst [], Lst [] -> 0
  | Lst (_ :: _), Lst [] -> 1
  | Lst (a :: aas), Lst (b :: bbs) -> (
      match compare_packets a b with
      | n when n = 0 -> compare_packets (Lst aas) (Lst bbs)
      | n -> n)

let parse_digit chars =
  let digits_str, rest = List.split_while chars ~f:Char.is_digit in
  let digit = Int.of_string (String.of_char_list digits_str) in
  Some (digit, rest)

let rec parse_inside_list chars : (packet list * char list) option =
  match chars with
  | ']' :: rest -> Some ([], rest)
  | _ -> (
      match parse_unit chars with
      | None -> None
      | Some (this, rest) -> (
          match rest with
          | ',' :: rest -> (
              match parse_inside_list rest with
              | None -> None
              | Some (other, rest) -> Some (this :: other, rest))
          | ']' :: rest -> Some ([ this ], rest)
          | _ -> raise (Parse_error ("illegal " ^ String.of_char_list rest))))

and parse_unit chars : (packet * char list) option =
  match chars with
  | '[' :: rest -> (
      match parse_inside_list rest with
      | Some (lst, rest) -> Some (Lst lst, rest)
      | None -> None)
  | _ :: _ -> (
      match parse_digit chars with
      | Some (digit, rest) -> Some (Num digit, rest)
      | None -> None)
  | _ -> None

let parse_packet_exn ln =
  (* print_endline ln; *)
  match parse_unit (String.to_list ln) with
  | Some (p, []) -> p
  | _ -> raise (Parse_error ("could not parse: " ^ ln))

let () =
  let lines = Input.read_input_day_as_lines 13 in
  let (groups : packet list) =
    lines
    |> List.filter ~f:(fun ln -> not (String.is_empty ln))
    |> List.map ~f:parse_packet_exn
  in

  let groups_t : (packet * packet) list =
    groups |> List.chunks_of ~length:2
    |> List.map ~f:(fun grp ->
           match grp with
           | [ a; b ] -> (a, b)
           | _ -> raise (Parse_error "could not get groups"))
  in

  let idx =
    List.filter_mapi groups_t ~f:(fun i (a, b) ->
        if compare_packets a b < 0 then Some (i + 1) else None)
  in
  printf "Part 1: %d\n" (List.fold ~init:0 ~f:Int.( + ) idx);

  let div1 = parse_packet_exn "[[2]]" in
  let div2 = parse_packet_exn "[[6]]" in
  let sorted = List.sort (div1 :: div2 :: groups) ~compare:compare_packets in
  let idx2 =
    List.filter_mapi sorted ~f:(fun i p ->
        if compare_packets p div1 = 0 || compare_packets p div2 = 0 then
          Some (i + 1)
        else None)
  in
  printf "Part 2: %d\n" (List.fold ~init:1 ~f:Int.( * ) idx2)
