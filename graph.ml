open Core.Std
open Map_data_t

(* TODO where to put this, here is not the right place *)
module PortComp = Comparator.Make(struct
    type t = Map_data_t.port

    let sexp_of_t p = String.sexp_of_t p.name
    let compare p1 p2 = String.compare p1.name p2.name
end)

type fancy_map_type =
    (port, (port * int) list, PortComp.comparator_witness) Map.t

type t = {
    strings_to_ports : Map_data_t.port String.Map.t;
    ports_to_routes : fancy_map_type;
}

(* TODO make this a fold *)
let rec add_all_ports {strings_to_ports; ports_to_routes} ports = match ports with
    | []          -> {strings_to_ports; ports_to_routes}
    | port::ports ->
            let new_map = Map.add strings_to_ports port.code port in
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
    ports_to_routes = Map.empty ~comparator:PortComp.comparator;
}

let from {data_source; metros; routes} =
    let e = empty () in
    let fst = add_all_ports e metros in
    add_all_routes fst routes

let all_ports {strings_to_ports;_} =
    List.map ~f:(fun (s,p) -> p)
    (Map.to_alist strings_to_ports)

let routes_from_port {ports_to_routes;_} p =
    let routes = Map.find ports_to_routes p in
    match routes with
    | None -> failwith "what"
    | Some routes -> routes

let port_for_code g str = Map.find g.strings_to_ports str
