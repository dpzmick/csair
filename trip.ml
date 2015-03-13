open Core.Std
open Port
open Graph

type t = Port.t list

let t_of_code_list lst ~on =
    let lst = List.map lst ~f:(Graph.port_for_code on) in
    match List.exists lst ~f:(Option.is_none) with
    | true -> None
    | false -> Some (List.map lst ~f:(fun e -> Option.value_exn e))

(* this is beautiful (tears) *)
let rec valid_on_graph ps ~on =
    match ps with
    | []         -> true
    | p::[]      -> (Graph.has_port on p)
    | p1::p2::ps ->
            match (Graph.has_port on p1) && (Graph.has_port on p2) with
            | false -> false
            | true  ->
                    let p1_one_step = List.map (Graph.routes_from_port_exn on p1) ~f:Route.to_port in
                    let local = List.exists p1_one_step ~f:(Port.equal p2) in
                    local && (valid_on_graph (p2::ps) ~on)


let shortest_route_bt p1 p2 ~on =
    let rs = (Graph.routes_from_port_exn on p1) in
    let possibles = List.filter rs ~f:(fun r -> Port.equal p2 (Route.to_port r)) in
    match List.length possibles with
    | 0 -> None
    | 1 -> Some (List.hd_exn possibles)
    | _ -> Some (List.hd_exn (List.sort possibles ~cmp:Route.compare))

let cost_on_graph_exn ps ~on =
    let rec aux remain leg_cost acc =
        let leg_cost = if (Float.(<) leg_cost 0.0) then 0.0 else leg_cost in
        match remain with
        | []         -> acc
        | _::[]      -> acc
        | p1::p2::ps ->
                match shortest_route_bt p1 p2 ~on with
                | None       -> failwith "invalid trip on given graph"
                | Some route -> aux (p2::ps)
                                    (leg_cost -. 0.05)
                                    (acc +. leg_cost *. (Float.of_int (Route.distance route)))
    in aux ps 0.35 0.0

let distance_on_graph_exn ps ~on =
    let rec aux remain acc =
        match remain with
        | []         -> acc
        | _::[]      -> acc
        | p1::p2::ps ->
                match shortest_route_bt p1 p2 ~on with
                | None       -> failwith "invalid trip on given graph"
                | Some route -> aux (p2::ps) (acc + (Route.distance route))
    in aux ps 0


let time_to_travel_on_graph_exn ps ~on =
    let time_for_uni_accel_from_zero tov dx = (1.0 /. (tov /. 2.0)) *. dx in
    let flight_time r =
        let fd = (Float.of_int (Route.distance r)) in
        if Float.(<) fd 400.0
        then 2.0 *. (time_for_uni_accel_from_zero 750.0 (fd /. 2.0))
        else
            let start_and_finish = time_for_uni_accel_from_zero 750.0 400.0 in
            let remain_dist = fd -. 400.0 in
            let rem_time = (1.0 /. 750.0) *. remain_dist in
            start_and_finish +. rem_time
    in
    let layover_time port =
        let routes = (Graph.routes_from_port_exn on port) in
        let max_time = 2.0 *. 60.0 in
        let attempted_time = max_time -. 10.0 *. (Float.of_int (List.length routes)) in
        if (Float.(<) 0.0 attempted_time)
        then 0.0
        else attempted_time
    in
    let rec aux remain acc =
        match remain with
        | []         -> acc
        | _::[]      -> acc
        | p1::p2::ps ->
                match shortest_route_bt p1 p2 ~on with
                | None       -> failwith "invalid trip on given graph"
                | Some route ->
                        let lt = match List.length ps with
                        | 0 -> 0.0
                        | _ -> layover_time p2
                        in
                        aux (p2::ps) (acc +. (flight_time route) +. lt)
    in
    aux ps 0.0
