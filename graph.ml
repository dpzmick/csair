open Core.Std
include Map_data_t

type t = Route.t list Port.Map.t

module Edit = struct
    type t =
        | PortEdit    of (string * string * string)
        | PortDelete  of string
        | PortAdd     of string
        | RouteEdit   of (string * string * string)
        | RouteDelete of (string * string * string option)
        | RouteAdd    of (string * string * string)

    let port_edit ~code ~field ~value = PortEdit (code,field,value)
    let port_delete code = PortDelete code
    let port_add code = PortAdd code

    let route_edit ~from_code ~to_code ~new_dist = RouteEdit (from_code, to_code, new_dist)

    let route_delete ~from_code ~to_code ~dist =
        match dist with
        | "" -> RouteDelete (from_code, to_code, None)
        | s  -> RouteDelete (from_code, to_code, Some s)

    let route_add ~from_code ~to_code ~dist = RouteAdd (from_code, to_code, dist)
end

module EditResult = struct
    type 'a t = ('a option * string option)

    let create v = (Some v, None)
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

let all_ports ports_to_routes = Map.keys ports_to_routes

let routes_from_port ports_to_routes p =
    let routes = Map.find ports_to_routes p in
    match routes with
    | None -> failwith "what"
    | Some routes -> routes

let all_routes ports_to_routes =
    Map.fold ports_to_routes
        ~init:[]
        ~f:(fun ~key:_ ~data:routes acc -> acc @ routes)

let t_of_dataset {metros; routes; directed;_} =
    add_all_routes (add_all_ports (empty ()) metros) routes directed

let dataset_of_t g =
    let metros = (all_ports g) in
    let routes = List.map (all_routes g) ~f:Route.to_json_route in
    {routes;metros;directed = false; data_source = []}

(**********************************************************************
*                        editing starts here                         *
**********************************************************************)
let edit_port g ~code ~field ~value =
    (* TODO this is kind of gross but I'm not sure how else to do it *)
    let old_port = port_for_code g code in
    match old_port with
    | None    -> EditResult.fail "port does not exist"
    | Some op ->
            try
                (* TODO make a better graph, should refactor so these edits are underling graph agnostic *)
                let curr_routes = Map.find_exn g op in
                let new_port = Port.modify_old op ~field ~value in
                let new_routes = List.map curr_routes
                        ~f:(fun r -> Route.create new_port (Route.to_port r) (Route.distance r)) in
                EditResult.create (Map.add g ~key:new_port ~data:new_routes)
            with
            | Failure "int_of_string"     -> EditResult.fail "need integer number"
            | Invalid_argument "float"    -> EditResult.fail "need floating point number"
            | Invalid_argument "negative" -> EditResult.fail "number must be positive"
            | Not_found                   -> EditResult.fail "field does not exist"

let remove_port g code =
    let port_to_rm = (port_for_code g code) in
    match port_to_rm with
    | None   -> EditResult.fail "port does not exist"
    | Some p ->
            let g = Map.remove g p in (* do this first to speed things up *)
            let affected_routes = List.filter (all_routes g) ~f:(fun r ->
                (Port.equal p (Route.from_port r)) || (Port.equal p (Route.to_port r)))
            in
            EditResult.create
                (List.fold affected_routes
                    ~init:g
                    ~f:(fun acc r ->
                        let start = (Route.from_port r) in
                        let curr_routes = Map.find_exn acc start in
                        let without = List.filter curr_routes ~f:(fun rr -> (not (Route.equal r rr))) in
                        Map.add acc ~key:start ~data:without))

let add_port g code =
    let new_port = Port.default_of_code code in
    match List.exists (all_ports g) ~f:(fun p1 -> String.equal (Port.code p1) code) with
    | false -> EditResult.create (add_all_ports g [new_port])
    | true  -> EditResult.fail "port already exists"

let edit_route g ~from_code ~to_code ~new_dist_string =
    let after_checks new_dist sp dp old_route =
        let curr_routes = Map.find_exn g sp in
        let without_routes = List.filter curr_routes ~f:(fun e -> not (Route.equal e old_route)) in
        let new_route = Route.create sp dp new_dist in
        let new_routes = new_route::without_routes in
        EditResult.create (Map.add g ~key:sp ~data:new_routes)
    in
    try
        let new_dist = int_of_string new_dist_string in
        if new_dist < 0
        then
            EditResult.fail "new distance must be positive"
        else
            let old_start = port_for_code g from_code in
            match old_start with
            | None           -> EditResult.fail "start port does not exist"
            | Some old_start ->
                    let old_end = port_for_code g to_code in
                    match old_end with
                    | None         -> EditResult.fail "end port does not exist"
                    | Some old_end ->
                            let route = List.find (all_routes g) ~f:(fun r ->
                                let s = (Port.code (Route.from_port r)) in
                                let e = (Port.code (Route.to_port r)) in
                                (String.equal s from_code) && (String.equal e to_code))
                            in match route with
                            | None       -> EditResult.fail "route does not exist"
                            | Some route -> after_checks new_dist old_start old_end route
    with
    | Failure "int_of_string" -> EditResult.fail "new distance must be an integer"

let remove_route g ~from_code ~to_code ~dist =
    let do_removal sp r =
        let curr = Map.find_exn g sp in
        let without = List.filter curr ~f:(fun rr -> (not (Route.equal r rr))) in
        EditResult.create (Map.add g ~key:sp ~data:without)
    in
    let dist_given dist sp dp =
        try
            let dist = int_of_string dist in
            let my_route = Route.create sp dp dist in
            let r = List.find (all_routes g) ~f:(Route.equal my_route) in
            match r with
            | None   -> EditResult.fail "route does not exist"
            | Some r -> do_removal sp r
        with
        | Failure "int_of_string" -> EditResult.fail "distance not an integer"

    in
    let no_dist_given sp dp =
        let rs = List.filter (all_routes g) ~f:(fun r ->
            let a = (Port.equal sp (Route.from_port r)) in
            let b = (Port.equal dp (Route.to_port r)) in
            a && b)
        in match (List.length rs) with
        | 0 -> EditResult.fail "route does not exist"
        | 1 -> do_removal sp (List.hd_exn rs)
        | _ -> EditResult.fail "multiple routes exist, need to specify a distance"

    in
    match (port_for_code g from_code) with
    | None    -> EditResult.fail "start port does not exist"
    | Some sp ->
            match (port_for_code g to_code) with
            | None    -> EditResult.fail "end port does not exist"
            | Some dp ->
                match dist with
                | None      -> no_dist_given sp dp
                | Some dist -> dist_given dist sp dp

(* TODO: abstract out all the map stuff (create an add routes that takes real routes, not json routes) *)
let add_route g source dest dist_string =
    let after_checks sp new_route =
        let curr = Map.find_exn g sp in (* already checked if it was in there *)
        let nmap = Map.add g ~key:sp ~data:(new_route::curr) in
        EditResult.create nmap
    in
    try
        let dist = int_of_string dist_string in
        if dist <= 0
        then EditResult.fail "distance must be greater than 0"
        else
            if (String.equal source dest)
            then EditResult.fail "source and dest are the same port"
            else
                match (port_for_code g source) with
                | None    -> EditResult.fail "source doesn't exist"
                | Some sp ->
                        match (port_for_code g dest) with
                        | None    -> EditResult.fail "dest doesn't exist"
                        | Some dp ->
                                let new_route = Route.create sp dp dist in
                                match List.find (all_routes g) ~f:(Route.equal new_route) with
                                | None   -> after_checks sp new_route
                                | Some _ -> EditResult.fail "this route already exists"
    with
    | Failure "int_of_string" -> EditResult.fail "distance not an integer"

let edit g edit =
    let open Edit in
    match edit with
    | PortEdit (code,field,value)            -> edit_port g ~code ~field ~value
    | PortDelete code                        -> remove_port g code
    | PortAdd code                           -> add_port g code
    | RouteEdit (from_code,to_code,new_dist) -> edit_route g ~from_code ~to_code ~new_dist_string:new_dist
    | RouteDelete (from_code,to_code,dist)   -> remove_route g ~from_code ~to_code ~dist
    | RouteAdd (source,dest,dist)            -> add_route g source dest dist
