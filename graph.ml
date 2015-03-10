open Core.Std
include Map_data_t

type t = Route.t list Port.Map.t

module Edit = struct
    type t =
        | PortEdit    of (string * string * string)
        | PortDelete  of string
        | PortAdd     of string
        | RouteEdit   of (string * string * string)
        | RouteDelete of (string * string)
        | RouteAdd    of (string * string * string)

    let port_edit code field value = PortEdit (code,field,value)
    let port_delete code = PortDelete code
    let port_add code = PortAdd code

    let route_edit source dest new_dist = RouteEdit (source,dest,new_dist)
    let route_delete source dest = RouteDelete (source,dest)
    let route_add source dest dist = RouteAdd (source,dest,dist)
end

module EditResult = struct
    type 'a t = ('a option * string option)

    let create v = (v, None)
    let fail s = (None, Some s)

    let new_graph (g, _) = g

    let failure_reason (_, s) =
        match s with
        | None    -> ""
        | Some s -> s
end

let port_for_code ports_to_routes str =
    let ports = Map.keys ports_to_routes in
    List.find ports ~f:(fun p -> String.equal (Port.code p) str)

let routes_from ports_to_routes port =
    match Map.find ports_to_routes port with
    | None    -> []
    | Some rs -> rs

(** Adds the ports with empty route lists *)
let add_all_ports g ports =
    List.fold ports
        ~init:g
        ~f:(fun map port -> Map.add map ~key:port ~data:[])

(** Adds all the routes, assumes all the routes start and end at ports already in the dictionary *)
let add_all_routes g json_routes directed =
    (* make a list of all the routes in the list of json_routes (make sure both directions exist) *)
    let routes =
        List.fold json_routes
            ~init:[]
            ~f:(fun acc json_route ->
                let port_one = (Option.value_exn (port_for_code g (List.nth_exn (json_route.ports) 0))) in
                let port_two = (Option.value_exn (port_for_code g (List.nth_exn (json_route.ports) 1))) in
                let distance = json_route.distance in
                let new_routes =
                    match directed with
                    | true  -> [Route.create port_one port_two distance]
                    | false -> [Route.create port_one port_two distance; Route.create port_two port_one distance]
                in
                new_routes @ acc
            )
    in
    (* Now we need to build up a new map with all of these routes in it *)
    let new_map =
        List.fold routes
            ~init:g
            ~f:(fun map route ->
                match Map.find map (Route.from_port route) with
                | None        -> Map.add map ~key:(Route.from_port route) ~data:[route]
                | Some routes -> Map.add map ~key:(Route.from_port route) ~data:(route::routes)
            )
    in new_map

let empty () = Port.Map.empty

let t_of_dataset {metros; routes; directed;_} =
    add_all_routes (add_all_ports (empty ()) metros) routes directed

let all_ports ports_to_routes = Map.keys ports_to_routes

let routes_from_port ports_to_routes p =
    let routes = Map.find ports_to_routes p in
    match routes with
    | None -> failwith "what"
    | Some routes -> routes

let all_routes ports_to_routes =
    Map.fold ports_to_routes
        ~init:[]
        ~f:(fun ~key:port ~data:routes acc -> acc @ routes)

let add_port g code =
    let new_port = Port.default_of_code code in
    match List.exists (all_ports g) ~f:(fun p1 -> String.equal (Port.code p1) code) with
    | false -> EditResult.create (Some (add_all_ports g [new_port]))
    | true  -> EditResult.fail "port already added"

let add_route g source dest dist_string =
    try
        let dist = int_of_string dist_string in
        if dist <= 0
        then EditResult.fail "distance must be greater than 0"
        else
            if (String.equal source dest)
            then EditResult.fail "source and dest are the same port"
            else
                let source_port = (port_for_code g source) in
                match source_port with
                | None    -> EditResult.fail "source doesn't exist"
                | Some sp ->
                        let dest_port = (port_for_code g dest) in
                        match dest_port with
                        | None    -> EditResult.fail "dest doesn't exist"
                        | Some dp -> (* now we can actually add the route *)
                                let new_route = Route.create sp dp dist in
                                let curr = Map.find_exn g sp in (* already checked if it was in there *)
                                let nmap = Map.add g ~key:sp ~data:(new_route::curr) in
                                EditResult.create (Some nmap)
    with
    | Failure "int_of_string" -> EditResult.fail "distance not a number"

let edit g edit =
    let open Edit in
    match edit with
    | PortEdit (code,field,value)       -> EditResult.create None
    | PortDelete code                   -> EditResult.create None
    | PortAdd code                      -> add_port g code
    | RouteEdit (source,dest,new_dist)  -> EditResult.create None
    | RouteDelete (source,dest)         -> EditResult.create None
    | RouteAdd (source,dest,dist)       -> add_route g source dest dist
