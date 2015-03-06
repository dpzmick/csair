open Core.Std
open Map_data_t

type t = {
    strings_to_ports : Port.t String.Map.t;
    ports_to_routes : (port * int) list Port.Map.t;
}

(* TODO make this a fold *)
let rec add_all_ports {strings_to_ports; ports_to_routes} ports = match ports with
    | []          -> {strings_to_ports; ports_to_routes}
    | port::ports ->
            let new_map = Map.add strings_to_ports (Port.code port) port in
            add_all_ports {strings_to_ports = new_map; ports_to_routes} ports

let routes_from ports_to_routes port =
    match Map.find ports_to_routes port with
    | None    -> []
    | Some rs -> rs

let add_route {strings_to_ports; ports_to_routes} ({ports; distance} : route) =
    let source              = Map.find_exn strings_to_ports (List.nth_exn ports 0) in
    let dest                = Map.find_exn strings_to_ports (List.nth_exn ports 1) in
    let new_routes_forward  = (dest, distance)::(routes_from ports_to_routes source) in
    let new_routes_backward = (source, distance)::(routes_from ports_to_routes dest) in
    let new_forward         = Map.add ports_to_routes source new_routes_forward  in
    let new_backward        = Map.add new_forward     dest   new_routes_backward in
    {strings_to_ports; ports_to_routes = new_backward}

(* TODO make this a fold *)
let rec add_all_routes g routes = match routes with
    | []    -> g
    | r::rs -> add_all_routes (add_route g r) rs

let empty () = {
    strings_to_ports = String.Map.empty;
    ports_to_routes = Map.empty ~comparator:Port.comparator;
}

let from {metros; routes;_} =
    let e = empty () in
    let fst = add_all_ports e metros in
    add_all_routes fst routes

let all_ports {strings_to_ports;_} =
    List.map ~f:(fun (_,p) -> p)
    (Map.to_alist strings_to_ports)

let routes_from_port {ports_to_routes;_} p =
    let routes = Map.find ports_to_routes p in
    match routes with
    | None -> failwith "what"
    | Some routes -> routes

let port_for_code g str = Map.find g.strings_to_ports str

let all_routes {ports_to_routes;_} = Map.to_alist ports_to_routes

(* TODO both of these can be a lot cleaner as well*)
let longest_path g =
    let rec aux = function
        | []                 -> 0
        | (_, reachable)::rs ->
                let max_reachable = List.max_elt (List.map reachable ~f:(fun (_,d) -> d)) ~cmp:Int.compare in
                match max_reachable with
                | None   -> aux rs
                | Some v -> Int.max v (aux rs)
    in aux (all_routes g)

let shortest_path g =
    let rec aux = function
        | []                 -> Int.max_value
        | (_, reachable)::rs ->
                let max_reachable = List.min_elt (List.map reachable ~f:(fun (_,d) -> d)) ~cmp:Int.compare in
                match max_reachable with
                | None   -> aux rs
                | Some v -> Int.min v (aux rs)
    in aux (all_routes g)

let largest_pop g =
    let pops = List.map (all_ports g) ~f:Port.population in
    match List.max_elt pops ~cmp:Int.compare with
    | None -> 0
    | Some v -> v

let smallest_pop g =
    let pops = List.map (all_ports g) ~f:Port.population in
    match List.min_elt pops ~cmp:Int.compare with
    | None -> 0
    | Some v -> v

let average_population g =
    let pops = List.map (all_ports g) ~f:Port.population in
    let sum = List.fold ~init:0 ~f:(+) pops in
    (float sum) /. (float (List.length pops))
