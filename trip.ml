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

let cost_on_graph_exn ps ~on =
    let rec aux remain leg_cost acc =
        let leg_cost = if (Float.(<) leg_cost 0.0) then 0.0 else leg_cost in
        match remain with
        | []         -> acc
        | p::[]      -> acc
        | p1::p2::ps ->
                let rs = (Graph.routes_from_port_exn on p1) in
                let possibles = List.filter rs ~f:(fun r -> Port.equal p2 (Route.to_port r)) in
                let route =
                    match List.length possibles with
                    | 0 -> failwith "invalid trip on given graph"
                    | 1 -> List.hd_exn possibles
                    | _ -> List.hd_exn (List.sort possibles ~cmp:Route.compare)
                in aux (p2::ps) (leg_cost -. 0.05) (acc +. leg_cost *. (Float.of_int (Route.distance route)))
    in aux ps 0.35 0.0

let distance_on_graph_exn ps ~on =
    let rec aux remain acc =
        match remain with
        | []         -> acc
        | p::[]      -> acc
        | p1::p2::ps ->
                let rs = (Graph.routes_from_port_exn on p1) in
                let possibles = List.filter rs ~f:(fun r -> Port.equal p2 (Route.to_port r)) in
                let route =
                    match List.length possibles with
                    | 0 -> failwith "invalid trip on given graph"
                    | 1 -> List.hd_exn possibles
                    | _ -> List.hd_exn (List.sort possibles ~cmp:Route.compare)
                in aux (p2::ps) (acc + (Route.distance route))
    in aux ps 0

let time_to_travel_on_graph_exn _ ~on = 0
