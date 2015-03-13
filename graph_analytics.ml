open Core.Std

let longest_path g  =
    List.max_elt (Graph.all_routes g)
        ~cmp:(fun r1 r2 -> Int.compare (Route.distance r1) (Route.distance r2))

let shortest_path g =
    List.min_elt (Graph.all_routes g)
        ~cmp:(fun r1 r2 -> Int.compare (Route.distance r1) (Route.distance r2))

let largest_pop g =
    let pops = List.map (Graph.all_ports g) ~f:Port.population in
    match List.max_elt pops ~cmp:Int.compare with
    | None -> 0
    | Some v -> v

let smallest_pop g =
    let pops = List.map (Graph.all_ports g) ~f:Port.population in
    match List.min_elt pops ~cmp:Int.compare with
    | None -> 0
    | Some v -> v

let average_population g =
    let pops = List.map (Graph.all_ports g) ~f:Port.population in
    let sum = List.fold ~init:0 ~f:(+) pops in
    (float sum) /. (float (List.length pops))

let continents_served g =
    let map =
        List.fold (Graph.all_ports g)
            ~init:String.Map.empty
            ~f:(fun acc port ->
                match Map.find acc (Port.continent port) with
                | None           -> Map.add acc ~key:(Port.continent port) ~data:[(Port.name port)]
                | Some countries -> Map.add acc ~key:(Port.continent port) ~data:((Port.name port)::countries))
    in (Map.to_alist map)

let hubs g =
    let map =
        List.fold (Graph.all_ports g)
            ~init:Int.Map.empty
            ~f:(fun acc port ->
                let len = List.length (Graph.routes_from_port_exn g port) in
                match Map.find acc len with
                | None         -> Map.add acc ~key:len ~data:[port]
                | Some sources -> Map.add acc ~key:len ~data:(port::sources)
            )
    in Map.max_elt map
