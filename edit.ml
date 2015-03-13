open Core.Std

type t =
    | PortEdit    of string * string * string
    | PortDelete  of string
    | PortAdd     of string
    | RouteEdit   of string * string * string
    | RouteDelete of string * string * string option
    | RouteAdd    of string * string * string

let port_edit ~code ~field ~value = PortEdit (code,field,value)
let port_delete code = PortDelete code
let port_add code = PortAdd code

let route_edit ~from_code ~to_code ~new_dist = RouteEdit (from_code, to_code, new_dist)

let route_delete ~from_code ~to_code ~dist =
    match dist with
    | "" -> RouteDelete (from_code, to_code, None)
    | s  -> RouteDelete (from_code, to_code, Some s)

let route_add ~from_code ~to_code ~dist = RouteAdd (from_code, to_code, dist)

let edit_port g ~code ~field ~value =
    let old_port = Graph.port_for_code g code in
    match old_port with
    | None    -> Edit_result.fail "port does not exist"
    | Some op ->
            try
                let curr_routes = Graph.routes_from_exn g op in
                let new_port = Port.modify_old op ~field ~value in
                let new_routes = List.map curr_routes
                        ~f:(fun r -> Route.create new_port (Route.to_port r) (Route.distance r)) in
                Edit_result.create (Graph.set_routes_from g ~port:new_port ~routes:new_routes)
            with
            | Failure "int_of_string"     -> Edit_result.fail "need integer number"
            | Invalid_argument "float"    -> Edit_result.fail "need floating point number"
            | Invalid_argument "negative" -> Edit_result.fail "number must be positive"
            | Not_found                   -> Edit_result.fail "field does not exist"

let remove_port g code =
    let port_to_rm = (Graph.port_for_code g code) in
    match port_to_rm with
    | None   -> Edit_result.fail "port does not exist"
    | Some p ->
            let g = Graph.without_port_no_cleanup g p in (* do this first *)
            let affected_routes = List.filter (Graph.all_routes g) ~f:(fun r ->
                (Port.equal p (Route.from_port r)) || (Port.equal p (Route.to_port r)))
            in
            (** TODO consider moving this logic into the graph *)
            Edit_result.create
                (List.fold affected_routes
                    ~init:g
                    ~f:(fun acc r ->
                        let start = (Route.from_port r) in
                        let curr_routes = Graph.routes_from_exn acc start in
                        let without = List.filter curr_routes ~f:(fun rr -> (not (Route.equal r rr))) in
                        Graph.set_routes_from acc ~port:start ~routes:without))

let add_port g code =
    let new_port = Port.default_of_code code in
    match List.exists (Graph.all_ports g) ~f:(fun p1 -> String.equal (Port.code p1) code) with
    | false -> Edit_result.create (Graph.add_all_ports g [new_port])
    | true  -> Edit_result.fail "port already exists"

let edit_route g ~from_code ~to_code ~new_dist_string =
    let after_checks new_dist sp dp old_route =
        let curr_routes = Graph.routes_from_exn g sp in
        let without_routes = List.filter curr_routes ~f:(fun e -> not (Route.equal e old_route)) in
        let new_route = Route.create sp dp new_dist in
        let new_routes = new_route::without_routes in
        Edit_result.create (Graph.set_routes_from g ~port:sp ~routes:new_routes)
    in
    try
        let new_dist = int_of_string new_dist_string in
        if new_dist < 0
        then
            Edit_result.fail "new distance must be positive"
        else
            let old_start = Graph.port_for_code g from_code in
            match old_start with
            | None           -> Edit_result.fail "start port does not exist"
            | Some old_start ->
                    let old_end = Graph.port_for_code g to_code in
                    match old_end with
                    | None         -> Edit_result.fail "end port does not exist"
                    | Some old_end ->
                            let route = List.find (Graph.all_routes g) ~f:(fun r ->
                                let s = (Port.code (Route.from_port r)) in
                                let e = (Port.code (Route.to_port r)) in
                                (String.equal s from_code) && (String.equal e to_code))
                            in match route with
                            | None       -> Edit_result.fail "route does not exist"
                            | Some route -> after_checks new_dist old_start old_end route
    with
    | Failure "int_of_string" -> Edit_result.fail "new distance must be an integer"

let remove_route g ~from_code ~to_code ~dist =
    (** TODO consider moving the do_removal function into the graph *)
    let do_removal sp r =
        let curr = Graph.routes_from_exn g sp in
        let without = List.filter curr ~f:(fun rr -> (not (Route.equal r rr))) in
        Edit_result.create (Graph.set_routes_from g ~port:sp ~routes:without)
    in
    let dist_given dist sp dp =
        try
            let dist = int_of_string dist in
            let my_route = Route.create sp dp dist in
            let r = List.find (Graph.all_routes g) ~f:(Route.equal my_route) in
            match r with
            | None   -> Edit_result.fail "route does not exist"
            | Some r -> do_removal sp r
        with
        | Failure "int_of_string" -> Edit_result.fail "distance not an integer"

    in
    let no_dist_given sp dp =
        let rs = List.filter (Graph.all_routes g) ~f:(fun r ->
            let a = (Port.equal sp (Route.from_port r)) in
            let b = (Port.equal dp (Route.to_port r)) in
            a && b)
        in match (List.length rs) with
        | 0 -> Edit_result.fail "route does not exist"
        | 1 -> do_removal sp (List.hd_exn rs)
        | _ -> Edit_result.fail "multiple routes exist, need to specify a distance"

    in
    match (Graph.port_for_code g from_code) with
    | None    -> Edit_result.fail "start port does not exist"
    | Some sp ->
            match (Graph.port_for_code g to_code) with
            | None    -> Edit_result.fail "end port does not exist"
            | Some dp ->
                match dist with
                | None      -> no_dist_given sp dp
                | Some dist -> dist_given dist sp dp

let add_route g source dest dist_string =
    let after_checks sp new_route =
        let curr = Graph.routes_from_exn g sp in (* already checked if it was in there *)
        let nmap = Graph.set_routes_from g ~port:sp ~routes:(new_route::curr) in
        Edit_result.create nmap
    in
    try
        let dist = int_of_string dist_string in
        if dist <= 0
        then Edit_result.fail "distance must be greater than 0"
        else
            if (String.equal source dest)
            then Edit_result.fail "source and dest are the same port"
            else
                match (Graph.port_for_code g source) with
                | None    -> Edit_result.fail "source doesn't exist"
                | Some sp ->
                        match (Graph.port_for_code g dest) with
                        | None    -> Edit_result.fail "dest doesn't exist"
                        | Some dp ->
                                let new_route = Route.create sp dp dist in
                                match List.find (Graph.all_routes g) ~f:(Route.equal new_route) with
                                | None   -> after_checks sp new_route
                                | Some _ -> Edit_result.fail "this route already exists"
    with
    | Failure "int_of_string" -> Edit_result.fail "distance not an integer"

let apply_to g edit =
    match edit with
    | PortEdit (code,field,value)            -> edit_port g ~code ~field ~value
    | PortDelete code                        -> remove_port g code
    | PortAdd code                           -> add_port g code
    | RouteEdit (from_code,to_code,new_dist) -> edit_route g ~from_code ~to_code ~new_dist_string:new_dist
    | RouteDelete (from_code,to_code,dist)   -> remove_route g ~from_code ~to_code ~dist
    | RouteAdd (source,dest,dist)            -> add_route g source dest dist
