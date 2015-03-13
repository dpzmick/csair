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
    | p1::p2::rs ->
            match (Graph.has_port on p1) && (Graph.has_port on p2) with
            | false -> false
            | true  ->
                    let p1_one_step = List.map (Graph.routes_from_port_exn on p1) ~f:Route.to_port in
                    let local = List.exists p1_one_step ~f:(Port.equal p2) in
                    local && (valid_on_graph (p2::rs) ~on)

let cost_on_graph _ ~on:_ = 0

let distance_on_graph ps ~on = 0

let time_to_travel_on_graph _ ~on = 0
