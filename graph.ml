open Core.Std
include Map_data_t

type t = Route.t list Port.Map.t

module Edit = struct
    type t =
        | PortEdit of (string * string * string)
        | PortDelete of string
        | PortAdd of int
        | RouteEdit of int
        | RouteDelete of int
        | RouteAdd of int

    let port_edit code field value = PortEdit (code,field,value)
    let port_delete code = PortDelete code
end

(* TODO expand type with error reporting *)
module EditResult = struct
    type 'a t = 'a option
    let create v = v
    let new_graph t = t
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

let edit g edit =
    let open Edit in
    match edit with
    | PortEdit (code,field,value) -> EditResult.create None
    | PortDelete code             -> EditResult.create None
