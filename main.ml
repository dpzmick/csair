open Core.Std
open Map_data_t
open Sys

(**
 * A highly imperative repl for querying and modifying the graph data
 * TODO this is literally abusive pls do something different
 *
 * ideas for editing (that aren't this, to give better errors)
 * -> create "edit" type (and query type)
 *      -> same as transactional thing in chess (make module submodule of graph and stuff)
 * -> How to pass everything though the graph with out being gross??
 *      -> if using above, then can just do match over type of edit in graph and behave appropriately
 **)

let list_all_cities g =
    List.iter ~f:(fun p -> printf "%s\n" p.name) (Graph.all_ports g)

let port_info_for g code =
    let port = Graph.port_for_code g code in
    match port with
    | None   -> printf "port %s not found!\n" code
    | Some p ->
            begin
                printf "port info for %s\n---\n%s\n" code (Port.string_of_t p);
                printf "Has flights to: \n";
                List.iter (Graph.routes_from_port_exn g p) ~f:(fun r ->
                    printf "\t%s\n" (Port.code (Route.to_port r)));
            end

let port_query g codes = List.iter codes ~f:(port_info_for g)

let print_continents g =
    List.iter (Graph_analytics.continents_served g)
        ~f:(fun (continent, cities) -> begin
            printf "Cities reachable on continent %s:\n" continent;
            List.iter cities
                ~f:(fun city -> printf "\t%s\n" city)
    end)

let print_hubs g =
    match (Graph_analytics.hubs g) with
    | None -> printf "No hubs\n"
    | Some (hub_connections, hubs) -> begin
        printf "Hubs with %d connections: " hub_connections;
        List.iter hubs ~f:(fun hub -> printf "%s, " (Port.name hub));
        printf "\n";
    end

let list_query g cmd =
    match cmd with
    | "hubs"::[] -> print_hubs g
    | "continents"::[] -> print_continents g
    | _ -> printf "command error: list query error\n"

let longest_single_flight_cmd g =
    let longest = Graph_analytics.longest_path g in
    match longest with
    | None -> printf "there was no longest flight\n"
    | Some r -> printf "Longest flight: %s -> %s, distance: %d\n"
                    (Port.code (Route.from_port r)) (Port.code (Route.to_port r)) (Route.distance r)

let biggest_query g cmd =
    match cmd with
    | "flight"::[] -> longest_single_flight_cmd g
    | "population"::[] -> printf "Largest Population: %d\n"  (Graph_analytics.largest_pop g);
    | _ -> printf "command error: error to greatest query\n"

let smallest_single_flight_cmd g =
    let longest = Graph_analytics.shortest_path g in
    match longest with
    | None -> printf "there was no shortest flight\n"
    | Some r -> printf "Shortest flight: %s -> %s, distance: %d\n"
                    (Port.code (Route.from_port r)) (Port.code (Route.to_port r)) (Route.distance r)

let smallest_query g cmd =
    match cmd with
    | "flight"::[] -> smallest_single_flight_cmd g
    | "population"::[] -> printf "Smallest Population: %d\n"  (Graph_analytics.smallest_pop g);
    | _ -> printf "command error: error to greatest query\n"

let average_query g cmd =
    match cmd with
    | "flight"::[] -> printf "not yet implemented\n"
    | "population"::[] -> printf "Average Population: %f\n"  (Graph_analytics.average_population g);
    | _ -> printf "command error: error to greatest query\n"

let trip_query on ps =
    match Trip.t_of_code_list ps ~on with
    | None    -> printf "trip is not valid"
    | Some t ->
            begin
                printf "trip cost: %f\n" (Trip.cost_on_graph_exn t ~on);
                printf "trip distance: %d\n" (Trip.distance_on_graph_exn t ~on);
                printf "trip travel time: %f\n" (Trip.time_to_travel_on_graph_exn t ~on)
            end

let query_cmd g cmd =
    begin
        match cmd with
        | "ports"::remain -> port_query g remain
        | "list"::remain -> list_query g remain
        | "biggest"::remain -> biggest_query g remain
        | "smallest"::remain -> smallest_query g remain
        | "average"::remain -> average_query g remain
        | "trip"::remain -> trip_query g remain
        | other::_ -> printf "command error: query %s is not a valid query\n" other
        | [] -> printf "command error: no query given\n"
    end;
    g (* "return" the same graph *)

let generic_edit g res_fun =
    let res = res_fun () in
    match Edit_result.new_graph res with
    | None -> printf "error doing update: %s\n" (Edit_result.failure_reason res); g
    | Some gg -> gg

let modify_port_cmd g cmd =
    let aux code field value () = Edit.apply_to g (Edit.port_edit ~code ~field ~value) in
    match cmd with
    | code::field::value1::values ->
            let value = List.fold values ~init:value1 ~f:(fun acc el -> String.concat ~sep:" " [acc;el]) in
            generic_edit g (aux code field value)
    | _::[]                   -> (printf "command error: need code field new_val\n"; g)
    | _                       -> (printf "command error: need code field new_val\n"; g)

let delete_port_cmd g cmd =
    let aux code () = Edit.apply_to g (Edit.port_delete code) in
    match cmd with
    | code::[] -> generic_edit g (aux code)
    | _        -> (printf "command error: need code\n"; g)

let add_port_cmd g cmd =
    let aux code () = Edit.apply_to g (Edit.port_add code) in
    match cmd with
    | code::[] -> generic_edit g (aux code)
    | _        -> (printf "command error: need code\n"; g)

let modify_route_cmd g cmd =
    let aux from_code to_code new_dist () =
        Edit.apply_to g (Edit.route_edit ~from_code ~to_code ~new_dist) in
    match cmd with
    | source::dest::new_dist::[]  -> generic_edit g (aux source dest new_dist)
    | _::[]                       -> (printf "command error: need source, dest, and new distance\n"; g)
    | _                           -> (printf "command error: need source, dest, and new distance\n"; g)

let delete_route_cmd g cmd =
    let aux1 from_code to_code () =
        Edit.apply_to g (Edit.route_delete ~from_code ~to_code ~dist:"") in
    let aux2 from_code to_code dist () =
        Edit.apply_to g (Edit.route_delete ~from_code ~to_code ~dist) in
    match cmd with
    | source::dest::[]       -> generic_edit g (aux1 source dest)
    | source::dest::dist::[] -> generic_edit g (aux2 source dest dist)
    | _                      -> (printf "command error: need source and dest (and maybe a dist)\n"; g)

let add_route_cmd g cmd =
    let aux source dest dist () = Edit.apply_to g (Edit.route_add source dest dist) in
    match cmd with
    | source::dest::dist::[] -> generic_edit g (aux source dest dist)
    | _                      -> (printf "command error: need source and dest\n"; g)


let edit_cmd g cmd =
    match cmd with
    | "port"::"modify"::remain -> modify_port_cmd g remain
    | "port"::"delete"::remain -> delete_port_cmd g remain
    | "port"::"add"::remain -> add_port_cmd g remain
    | "route"::"modify"::remain -> modify_route_cmd g remain
    | "route"::"delete"::remain -> delete_route_cmd g remain
    | "route"::"add"::remain -> add_route_cmd g remain
    | _ -> printf "command error: invalid edit command\n"; g

let map_cmd g cmd =
    begin
        match cmd with
        | [] -> printf "return value: %d\n"
                    (Sys.command (sprintf "open %s" (Gcm_data.string_of_t (Gcm_data.from g))))
        | _ -> printf "command error: the map command takes no arguments\n"
    end;
    g (* "return" the same graph *)

let write_cmd g cmd =
    begin
        match cmd with
        | filename::[] ->
                let dset = (Graph.dataset_of_t g) in
                Ag_util.Biniou.to_file Map_data_j.write_dataset filename dset
        | _::_::_      -> printf "command error: I'm not sure what you are trying to do man\n"
        | []           -> printf "command error: write command needs a filename\n"
    end;
    g

let merge_cmd g remain =
    match remain with
    | filename::[] ->
            let new_data = Ag_util.Json.from_file Map_data_j.read_dataset filename in
            Graph.merge_with_dataset g new_data
    | _ -> (printf "command error: merge command takes a file name"; g)

let command g cmd =
    match cmd with
    | "query"::remain -> query_cmd g remain
    | "edit"::remain  -> edit_cmd g remain
    | "map"::remain   -> map_cmd g remain
    | "write"::remain -> write_cmd g remain
    | "merge"::remain -> merge_cmd g remain
    | _ -> printf "error: command not recognized\n"; g (* in error, don't change anything *)

let () =
    let g = Graph.t_of_dataset (Ag_util.Json.from_file Map_data_j.read_dataset "map_data.json") in
    let g = ref g in
    let cont = ref true in
    while !cont do
        let () = printf "> %!" in
        let line = In_channel.input_line In_channel.stdin in
        match line with
        | None        -> cont := false
        | Some "exit" -> cont := false
        | Some cmd    -> g := command (!g) (String.split ~on:' ' cmd)
    done
