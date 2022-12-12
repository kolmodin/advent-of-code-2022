open Base

let bfs (cmp : ('rep, 'cmp) Comparator.Module.t) (initS : 'state)
    (rep : 'state -> 'rep) next stop =
  let rec go seen queue =
    match queue with
    | [] -> None
    | s :: queue' -> (
        match Set.mem seen (rep s) with
        | true -> go seen queue'
        | false ->
            if stop s then Some s
            else go (Set.add seen (rep s)) (queue' @ next s))
  in
  go (Set.empty cmp) [ initS ]
