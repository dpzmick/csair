open Core.Std
include Map_data_t

type t = Route.t list Port.Map.t

(** Finds the Port.t with a given code *)
let port_for_code g str =
    let ports = Map.keys g in
    List.find ports ~f:(fun p -> String.equal (Port.code p) str)

(** Gets all the routes from the specified port
 * returns an empty set if the given port does not exist *)
let routes_from g port =
    match Map.find g port with
    | None    -> []
    | Some rs -> rs

(** Adds the ports with empty route lists *)
let add_all_ports g ports =
    List.fold ports
        ~init:g
        ~f:(fun map port -> Map.add map ~key:port ~data:[])

(** Creates a Route.t from a json route, included here so we can look up ports (assuming they exist) *)
let routify_json_route g json_route =
    let from_port = (port_for_code g (List.nth_exn json_route.ports 0)) in
    match from_port with
    | None    -> None
    | Some fp -> (* lol because this is functional*)
            let to_port = (port_for_code g (List.nth_exn json_route.ports 1)) in
            match to_port with
            | None    -> None
            | Some tp -> Some (Route.create fp tp json_route.distance)

(** Creates a Route.t from a json route, included here so we can look up ports (assuming they exist)
 * Does this "backwards", the first port is used as the to_port and the second is used as from_port *)
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

(** Returns and empty t *)
let empty = Port.Map.empty

(** all the ports in the dataset *)
let all_ports g = Map.keys g

(** all the routes in the dataset *)
let all_routes g =
    Map.fold g
        ~init:[]
        ~f:(fun ~key:_ ~data:routes acc -> acc @ routes)

(** makes a t from a Map_data_t.datset *)
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

(** Makes a Map_data_t.dataset from t *)
(* TODO don't loose the data sources *)
let dataset_of_t g =
    let metros = (all_ports g) in
    let routes = List.map (all_routes g) ~f:Route.to_json_route in
    {routes;metros;directed = true; data_source = []}

let routes_from_port     g port = Map.find     g port
let routes_from_port_exn g port = Map.find_exn g port

let set_routes_from g ~port ~routes = Map.add g ~key:port ~data:routes

(** Removes the port from the dataset, without attempting to remove any connected routes or anything *)
let without_port_no_cleanup g port = Map.remove g port
