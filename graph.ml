open Core.Std
include Map_data_t

type t = {
    g: Route.t list Port.Map.t;
    ds: String.Set.t;
}

(** Finds the Port.t with a given code *)
let port_for_code {g;_} str =
    let ports = Map.keys g in
    List.find ports ~f:(fun p -> String.equal (Port.code p) str)

(** Gets all the routes from the specified port
 * returns an empty set if the given port does not exist *)
let routes_from {g;_} port =
    match Map.find g port with
    | None    -> []
    | Some rs -> rs

(** Adds the ports with empty route lists *)
let add_all_ports {g;ds} ports =
    let map =
        List.fold ports
            ~init:g
            ~f:(fun map port ->
                match Map.find g port with
                | None   -> Map.add map ~key:port ~data:[]
                | Some _ -> map) (* it's already in there *)
    in {g=map;ds}

(** Creates a Route.t from a json route, included here so we can look up ports (assuming they exist) *)
let routify_json_route g json_route =
    let from_port = (port_for_code g (List.nth_exn json_route.ports 0)) in
    match from_port with
    | None    -> None
    | Some fp -> (* lol because this is functional *)
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
let add_all_routes {g;ds} routes =
    let map =
        List.fold routes
            ~init:g
            ~f:(fun map route ->
                match Map.find map (Route.from_port route) with
                | None        -> Map.add map ~key:(Route.from_port route) ~data:[route]
                | Some routes -> Map.add map ~key:(Route.from_port route) ~data:(route::routes))
    in {g=map;ds}

(** Returns and empty t *)
let empty =  {g = Port.Map.empty; ds = String.Set.empty}

(** all the ports in the dataset *)
let all_ports {g;_} = Map.keys g

(** all the routes in the dataset *)
let all_routes {g;_} =
    Map.fold g
        ~init:[]
        ~f:(fun ~key:_ ~data:routes acc -> acc @ routes)

(** makes a t from a Map_data_t.datset *)
let t_of_dataset {metros; routes; directed;data_source} =
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
    in {(add_all_routes first real_routes) with ds=(String.Set.of_list data_source)}

let merge_with_dataset g {metros; routes; directed; data_source} =
    let first = (add_all_ports g metros) in
    let real_routes =
        let aux f = List.map routes ~f:(fun j ->
            match f first j with
            | None   -> failwith "json is malformed"
            | Some r -> r)
        in let uncond = aux routify_json_route in
        match directed with
        | true  -> uncond
        | false -> List.append uncond (aux routify_json_route_backwards)
    in
    let new_ds =
        List.fold data_source
            ~init:g.ds
            ~f:(fun acc el -> String.Set.add acc el)
    in
    {(add_all_routes first real_routes) with ds=new_ds}

(** Makes a Map_data_t.dataset from t *)
(* TODO don't loose the data sources *)
let dataset_of_t g =
    let metros = (all_ports g) in
    let routes = List.map (all_routes g) ~f:Route.to_json_route in
    {routes;metros;directed = true; data_source = String.Set.to_list g.ds}

let routes_from_port     {g;_} port = Map.find     g port
let routes_from_port_exn {g;_} port = Map.find_exn g port

let set_routes_from {g;ds} ~port ~routes =
    let map = Map.add g ~key:port ~data:routes in
    {g=map;ds}

(** Removes the port from the dataset, without attempting to remove any connected routes or anything *)
let without_port_no_cleanup {g;ds} port =
    {g = Map.remove g port; ds}

let has_port g p = List.exists (all_ports g) ~f:(Port.equal p)
