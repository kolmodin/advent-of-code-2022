open Base
open Stdio

exception Parse_error of string

type file = { size : int; name : string; path : string list }

let discover_files =
  let rec parse_cmd path lines =
    match lines with
    | [] -> []
    | ln :: rest -> (
        match String.split_on_chars ln ~on:[ ' ' ] with
        | [ "$"; "cd"; "/" ] -> parse_cmd [ "/" ] rest
        | [ "$"; "cd"; ".." ] -> parse_cmd (List.tl_exn path) rest
        | [ "$"; "ls" ] -> parse_ls path rest
        | [ "$"; "cd"; name ] -> parse_cmd (name :: path) rest
        | _ -> raise (Parse_error ("could not parse command: " ^ ln)))
  and parse_ls path lines =
    match lines with
    | [] -> []
    | ln :: rest -> (
        match String.split_on_chars ln ~on:[ ' ' ] with
        | "dir" :: _name -> parse_ls path rest
        | "$" :: _ -> parse_cmd path lines
        | [ size_str; name ] ->
            { size = Int.of_string size_str; name; path = List.rev path }
            :: parse_ls path rest
        | _ -> raise (Parse_error ("could not parse ls: " ^ ln)))
  in
  parse_cmd []

type dir = {
  files : (string, int, Base.String.comparator_witness) Map.t;
  sub_dirs : (string, dir, Base.String.comparator_witness) Map.t;
  total_size : int;
}

let empty_dir : dir =
  {
    files = Map.empty (module String);
    sub_dirs = Map.empty (module String);
    total_size = 0;
  }

let rec add_to_tree dir ~path ~name ~size =
  match path with
  | [] -> { dir with files = Map.add_exn dir.files ~key:name ~data:size }
  | root :: path' ->
      {
        dir with
        sub_dirs =
          Map.update dir.sub_dirs root ~f:(fun sub_tree ->
              add_to_tree
                (Option.value sub_tree ~default:empty_dir)
                ~path:path' ~name ~size);
      }

let sumi = List.fold ~init:0 ~f:Int.( + )

let rec iter_dirs path dir ~f =
  f path dir;
  Map.iteri dir.sub_dirs ~f:(fun ~key:name ~data:sub_dir ->
      iter_dirs (path ^ "/" ^ name) sub_dir ~f)

let rec update_size_of_tree dir : dir =
  let this_dir_size = sumi (Map.data dir.files) in
  let sub_dirs = Map.map dir.sub_dirs ~f:update_size_of_tree in
  let sub_dirs_size =
    sumi (Map.data sub_dirs |> List.map ~f:(fun d -> d.total_size))
  in
  { dir with sub_dirs; total_size = this_dir_size + sub_dirs_size }

let build_tree (files : file list) : dir =
  update_size_of_tree
    (List.fold files ~init:empty_dir ~f:(fun dir file ->
         add_to_tree dir ~path:file.path ~name:file.name ~size:file.size))

let count_dirs dir =
  let size = ref 0 in
  iter_dirs "" dir ~f:(fun _path dir ->
      if dir.total_size <= 100000 then size := !size + dir.total_size else ());
  !size

let find_smallest_dir_larger_than dir larger_or_eq_than =
  let smallest = ref dir.total_size in
  iter_dirs "" dir ~f:(fun _path dir ->
      if dir.total_size >= larger_or_eq_than && dir.total_size < !smallest then
        smallest := dir.total_size
      else ());
  !smallest

let () =
  let lines = Aoc.Input.read_input_day_as_lines 7 in
  let files = discover_files lines in
  let dir = build_tree files in
  print_endline ("Part 1: " ^ Int.to_string (count_dirs dir));
  let free_space : int = 70000000 - dir.total_size in
  let missing_space = 30000000 - free_space in
  print_endline ("Free space: " ^ Int.to_string free_space);
  print_endline ("Missing space: " ^ Int.to_string missing_space);
  print_endline
    ("Part 2: "
    ^ Int.to_string (find_smallest_dir_larger_than dir missing_space))
