open Base
open Stdio

exception Parse_error of string
exception Error of string

type bin_op = Add | Sub | Mul | Div [@@deriving sexp]
type op = Value of int64 | BinOp of bin_op * string * string
type line = { name : string; op : op }

type eop = EValue of int64 | EBinOp of bin_op * eop * eop | EVar of string
[@@deriving sexp]

let parse_line ln =
  match
    Aoc.Input.split_by ln ~keep:(fun c ->
        not (List.mem [ ' '; ':' ] c ~equal:Char.equal))
  with
  | [ name; value ] -> { name; op = Value (Int64.of_string value) }
  | [ name; sub1; bin_op; sub2 ] ->
      let bop =
        match bin_op with
        | "+" -> Add
        | "-" -> Sub
        | "*" -> Mul
        | "/" -> Div
        | _ -> raise (Parse_error "could not parse binop")
      in
      { name; op = BinOp (bop, sub1, sub2) }
  | _ -> raise (Parse_error "could not parse line")

let eval name equations : int64 =
  let ht = Hashtbl.create (module String) in
  let rec eval name =
    match Hashtbl.find ht name with
    | Some x -> x
    | None ->
        let e = Hashtbl.find_exn equations name in
        let result =
          match e with
          | Value value -> value
          | BinOp (op, e1, e2) -> (
              let v1 = eval e1 in
              let v2 = eval e2 in
              match op with
              | Add -> Int64.( + ) v1 v2
              | Sub -> Int64.( - ) v1 v2
              | Mul -> Int64.( * ) v1 v2
              | Div -> Int64.( / ) v1 v2)
        in
        Hashtbl.set ht ~key:name ~data:result;
        result
  in
  eval name

let rec simplify name vars (ht : (string, op) Hashtbl.t) : eop =
  if List.mem vars name ~equal:String.equal then EVar name
  else
    match Hashtbl.find_exn ht name with
    | Value x -> EValue x
    | BinOp (bin_op, n1, n2) -> (
        let run op a b =
          match op with
          | Add -> Int64.( + ) a b
          | Sub -> Int64.( - ) a b
          | Mul -> Int64.( * ) a b
          | Div -> Int64.( / ) a b
        in
        match (simplify n1 vars ht, simplify n2 vars ht) with
        | EValue v1, EValue v2 -> EValue (run bin_op v1 v2)
        | e1, e2 -> EBinOp (bin_op, e1, e2))

let resolve_eq lhs rhs =
  let rec resolve value exp =
    let open Int64 in
    match exp with
    | EVar _ -> value
    | EValue exp -> raise (Error "expected EVar")
    | EBinOp (bin_op, EValue v1, e2) -> (
        match bin_op with
        | Add -> resolve (value - v1) e2
        | Sub -> resolve (v1 - value) e2
        | Mul -> resolve (value / v1) e2
        | Div -> resolve (value * v1) e2)
    | EBinOp (bin_op, e1, EValue v2) -> (
        match bin_op with
        | Add -> resolve (value - v2) e1
        | Sub -> resolve (value + v2) e1
        | Mul -> resolve (value / v2) e1
        | Div -> resolve (value * v2) e1)
    | EBinOp (_, _, _) -> raise (Error "expected ")
  in

  match (lhs, rhs) with
  | EValue value, exp -> resolve value exp
  | exp, EValue value -> resolve value exp
  | _ -> raise (Error "expected either lhs or rhs to be fully evaluated")

let () =
  let lines = Aoc.Input.get_input_parsed 21 ~parser:parse_line in

  let equations =
    lines
    |> List.map ~f:(fun ln -> (ln.name, ln.op))
    |> Hashtbl.of_alist_exn (module String)
  in

  printf !"Part 1: %Ld\n" (eval "root" equations);

  let lhs, rhs =
    Hashtbl.find_exn equations "root" |> function
    | BinOp (_, lhs, rhs) ->
        (simplify lhs [ "humn" ] equations, simplify rhs [ "humn" ] equations)
    | _ -> raise (Error "expected binary op at root")
  in
  let x = resolve_eq lhs rhs in
  printf "Part 2: %Ld\n" x
