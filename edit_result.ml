type 'a t = ('a option * string option)

let create v = (Some v, None)
let fail s = (None, Some s)

let new_graph (g, _) = g

let failure_reason (_, s) =
    match s with
    | None    -> ""
    | Some s -> s
