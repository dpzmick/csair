open Core.Std
include Map_data_t

type t = Route.t list Port.Map.t

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

let routify_json_route g json_route =
    let from_port = (port_for_code g (List.nth_exn json_route.ports 0)) in
    match from_port with
    | None    -> None
    | Some fp -> (* lol because this is functional*)
            let to_port = (port_for_code g (List.nth_exn json_route.ports 1)) in
            match to_port with
            | None    -> None
            | Some tp -> Some (Route.create fp tp json_route.distance)

let routify_json_route_backwards g json_route =
    let from_port = (port_for_code g (List.nth_exn json_route.ports 1)) in
    match from_port with
    | None    -> None
    | Some fp -> (* lol because this is functional*)
            let to_port = (port_for_code g (List.nth_exn json_route.ports 0)) in
            match to_port with
            | None    -> None
            | Some tp -> Some (Route.create fp tp json_route.distance)

(** Adds all the routes, assumes all the routes start and end at ports already in the dictionary *)
let add_all_routes g routes =
    List.fold routes
        ~init:g
        ~f:(fun map route ->
            match Map.find map (Route.from_port route) with
            | None        -> Map.add map ~key:(Route.from_port route) ~data:[route]
            | Some routes -> Map.add map ~key:(Route.from_port route) ~data:(route::routes))

let empty = Port.Map.empty

let all_ports ports_to_routes = Map.keys ports_to_routes

let routes_from_port ports_to_routes p =
    let routes = Map.find ports_to_routes p in
    match routes with
    | None -> failwith "what" (* TODO *)
    | Some routes -> routes

let all_routes ports_to_routes =
    Map.fold ports_to_routes
        ~init:[]
        ~f:(fun ~key:_ ~data:routes acc -> acc @ routes)

let t_of_dataset {metros; routes; directed;_} =
    let first = (add_all_ports empty metros) in
    let real_routes =
        let aux f = List.map routes ~f:(fun j ->
            match f first j with
            | None   -> failwith "json is malformed"
            | Some r -> r)
        in let uncond = aux routify_json_route in
        match directed with
        | true  -> uncond
        | false -> List.append uncond (aux routify_json_route_backwards)
    in add_all_routes first real_routes

(* TODO don't loose the data sources *)
let dataset_of_t g =
    let metros = (all_ports g) in
    let routes = List.map (all_routes g) ~f:Route.to_json_route in
    {routes;metros;directed = true; data_source = []}

let routes_from     g port = Map.find     g port
let routes_from_exn g port = Map.find_exn g port

let set_routes_from g ~port ~routes = Map.add g ~key:port ~data:routes

let without_port_no_cleanup g port = Map.remove g port
