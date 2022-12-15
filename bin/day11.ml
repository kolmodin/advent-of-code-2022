open Base
open Stdio

exception Internal_error of string

type op = Mul of int | Add of int | MulMul
type test = { div_by : int; if_true : int; if_false : int }
type monkey = { items : int list; op : op; test : test }

let init =
  [
    {
      (* monkey 0 *)
      items = [ 54; 98; 50; 94; 69; 62; 53; 85 ];
      op = Mul 13;
      test = { div_by = 3; if_true = 2; if_false = 1 };
    };
    {
      (* monkey 1 *)
      items = [ 71; 55; 82 ];
      op = Add 2;
      test = { div_by = 13; if_true = 7; if_false = 2 };
    };
    {
      (* Monkey 2 *)
      items = [ 77; 73; 86; 72; 87 ];
      op = Add 8;
      test = { div_by = 19; if_true = 4; if_false = 7 };
    };
    (* Monkey 3: *)
    {
      items = [ 97; 91 ];
      op = Add 1;
      test = { div_by = 17; if_true = 6; if_false = 5 };
    };
    (* Monkey 4: *)
    {
      items = [ 78; 97; 51; 85; 66; 63; 62 ];
      op = Mul 17;
      test = { div_by = 5; if_true = 6; if_false = 3 };
    };
    (* Monkey 5: *)
    {
      items = [ 88 ];
      op = Add 3;
      test = { div_by = 7; if_true = 1; if_false = 0 };
    };
    (* Monkey 6: *)
    {
      items = [ 87; 57; 63; 86; 87; 53 ];
      op = MulMul;
      test = { div_by = 11; if_true = 5; if_false = 0 };
    };
    (* Monkey 7: *)
    {
      items = [ 73; 59; 82; 65 ];
      op = Add 6;
      test = { div_by = 2; if_true = 4; if_false = 3 };
    };
  ]

let run_op op n =
  match op with Mul m -> n * m | Add m -> n + m | MulMul -> n * n

let add_to_monkey m item = { m with items = m.items @ [ item ] }
let part1_div = 3

let prod_of_divs =
  List.fold init ~init:part1_div ~f:(fun acc m -> acc * m.test.div_by)

let round arr div_by =
  let idxs = List.range 0 (Array.length arr) in
  List.map idxs ~f:(fun idx ->
      let this = Array.get arr idx in
      let items = this.items in
      List.iter items ~f:(fun it ->
          let it' = run_op this.op it / div_by % prod_of_divs in
          let receiver =
            if it' % this.test.div_by = 0 then this.test.if_true
            else this.test.if_false
          in
          Array.set arr receiver (add_to_monkey (Array.get arr receiver) it'));
      Array.set arr idx { this with items = [] };
      (idx, List.length items))

let rec rounds arr div_by n =
  if n = 0 then [] else round arr div_by @ rounds arr div_by (n - 1)

let calc_monkey_business div_by n =
  let monkeys = Array.of_list init in
  let freq = rounds monkeys div_by n in
  let freqMap = Map.of_alist_reduce (module Int) freq ~f:Int.( + ) in
  let active =
    List.sort (Map.to_alist freqMap) ~compare:(fun (_, v) (_, v2) ->
        compare v2 v)
  in
  match List.take active 2 with
  | [ (_, v1); (_, v2) ] -> v1 * v2
  | _ -> raise (Internal_error "error")

let () =
  printf "Part 1: %d\n" (calc_monkey_business part1_div 20);
  printf "Part 2: %d\n" (calc_monkey_business 1 10000)
