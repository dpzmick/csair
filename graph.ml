open Core.Std
include Map_data_t

type t = {
    strings_to_ports : Port.t String.Map.t;
    ports_to_routes : Route.t list Port.Map.t;
}

let port_for_code {strings_to_ports;_} str = String.Map.find strings_to_ports str

(* TODO make this a fold *)
let rec add_all_ports {strings_to_ports; ports_to_routes} ports = match ports with
    | []          -> {strings_to_ports; ports_to_routes}
    | port::ports ->
            let new_map = Map.add strings_to_ports ~key:(Port.code port) ~data:port in
            add_all_ports {strings_to_ports = new_map; ports_to_routes} ports

let routes_from ports_to_routes port =
    match Map.find ports_to_routes port with
    | None    -> []
    | Some rs -> rs

let add_all_routes g json_routes =
    (* make a list of all the routes in the list of json_routes (make sure both directions exist) *)
    let routes =
        List.fold json_routes
            ~init:[]
            ~f:(fun acc json_route ->
                let port_one = (Option.value_exn (port_for_code g (List.nth_exn (json_route.ports) 0))) in
                let port_two = (Option.value_exn (port_for_code g (List.nth_exn (json_route.ports) 1))) in
                let distance = json_route.distance in
                let new_routes = [Route.create port_one port_two distance; Route.create port_two port_one distance] in
                new_routes @ acc
            )
    in
    (* Now we need to build up a new map with all of these routes in it *)
    let new_map =
        List.fold routes
            ~init:(g.ports_to_routes)
            ~f:(fun map route ->
                match Map.find map (Route.from_port route) with
                | None        -> Map.add map ~key:(Route.from_port route) ~data:[route]
                | Some routes -> Map.add map ~key:(Route.from_port route) ~data:(route::routes)
            )
    in
    {
        strings_to_ports = g.strings_to_ports;
        ports_to_routes  = new_map;
    }

let empty () = {
    strings_to_ports = String.Map.empty;
    ports_to_routes = Port.Map.empty;
}

let t_of_dataset {metros; routes;_} =
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

let all_routes {ports_to_routes;_} =
    Map.fold ports_to_routes
        ~init:[]
        ~f:(fun ~key:port ~data:routes acc -> acc @ routes)

(* TODO extract many of these, they aren't really a part of the graph, they are some sort of analytics *)
let longest_path g  = List.max_elt (all_routes g) ~cmp:(fun r1 r2 -> Int.compare (Route.distance r1) (Route.distance r2))
let shortest_path g = List.min_elt (all_routes g) ~cmp:(fun r1 r2 -> Int.compare (Route.distance r1) (Route.distance r2))

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

let continents_served g =
    let map =
        List.fold (all_ports g)
            ~init:String.Map.empty
            ~f:(fun acc port ->
                match Map.find acc (Port.continent port) with
                | None           -> Map.add acc ~key:(Port.continent port) ~data:[(Port.name port)]
                | Some countries -> Map.add acc ~key:(Port.continent port) ~data:((Port.name port)::countries))
    in (Map.to_alist map)

let hubs g =
    let map =
        List.fold (all_ports g)
            ~init:Int.Map.empty
            ~f:(fun acc port ->
                let len = List.length (routes_from_port g port) in
                match Map.find acc len with
                | None         -> Map.add acc len [port]
                | Some sources -> Map.add acc len (port::sources)
            )
    in Map.max_elt map
