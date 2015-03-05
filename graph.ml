open Core.Std
open Map_data_t

(* TODO where to put this, here is not the right place *)
module PortComp = Comparator.Make(struct
    type t = Map_data_t.port

    let sexp_of_t p = String.sexp_of_t p.name
    let compare p1 p2 = String.compare p1.name p2.name
end)

type fancy_route = {
    dest: port;
    dist: int;
}

type fancy_map_type =
    (port, fancy_route list, PortComp.comparator_witness) Map.t

let add_route {strings_to_ports; ports_to_routes} {[source, dest], distance} =
    {strings_to_ports; Map.add 

type t = {
    strings_to_ports : Map_data_t.port String.Map.t;
    ports_to_routes : fancy_map_type;
}

let rec add_all_ports ports map = match ports with
    | []          -> map
    | port::ports -> add_all_ports ports (Map.add map port.code port)

let empty () = {
    strings_to_ports = String.Map.empty;
    ports_to_routes = Map.empty ~comparator:PortComp.comparator;
}

let from {data_source; metros; routes} = {
    strings_to_ports = add_all_ports metros String.Map.empty;
    ports_to_routes = Map.empty ~comparator:PortComp.comparator;
}

let all_ports {strings_to_ports;_} =
    List.map ~f:(fun (s,p) -> p)
    (Map.to_alist strings_to_ports)

let port_info_for g str = Map.find g.strings_to_ports str
