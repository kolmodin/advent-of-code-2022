exception Parse_error of string

let compartments line =
  let len = String.length line in
  assert (len mod 2 == 0);
  (String.sub line 0 (len / 2), String.sub line (len / 2) (len / 2))

let priority c =
  match c with
  | c when c >= 'a' && c <= 'z' -> Char.code c - Char.code 'a' + 1
  | c when c >= 'A' && c <= 'Z' -> Char.code c - Char.code 'A' + 27
  | c -> raise (Parse_error ("No priority for: " ^ Char.escaped c))

module SC = Set.Make (Char)

let in_all lst =
  let sets = List.map (fun str -> SC.of_seq (String.to_seq str)) lst in
  let shared = List.fold_left SC.inter (List.hd sets) (List.tl sets) in
  match List.of_seq (SC.to_seq shared) with [ x ] -> Some x | _ -> None

let read_file_to_single_string filename =
  In_channel.with_open_text filename In_channel.input_all

let split_lines str =
  let rec drop_last_if_empty (lst : string list) =
    match lst with
    | [] -> []
    | "" :: [] -> []
    | x :: [] ->
        print_endline "WARNING: last line of input not empty";
        [ x ]
    | x :: xs -> x :: drop_last_if_empty xs
  in
  drop_last_if_empty (String.split_on_char '\n' str)

let sumi = List.fold_left Int.add 0

let rec take n lst =
  match lst with
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

let rec drop n lst =
  match lst with
   | _ when n <= 0 -> lst
   | [] -> []
   | _ :: xs -> drop (n - 1) xs

let rec chunks n lst =
  match lst with [] -> [] | _ -> take n lst :: chunks n (drop n lst)

let () =
  let lines = split_lines (read_file_to_single_string "day03.txt") in
  let prios =
    Fun.flip List.map lines @@ fun ln ->
    let a, b = compartments ln in
    let (Some c) = in_all [ a; b ] in
    priority c
  in
  print_endline ("Part 1: " ^ string_of_int (sumi prios));
  let prios2 =
    Fun.flip List.map (chunks 3 lines) @@ fun group ->
    let (Some c) = in_all group in
    priority c
  in
  print_endline ("Part 2: " ^ string_of_int (sumi prios2))
