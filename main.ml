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
    | Some p -> printf "port info for %s\n---\n%s\n" code (Port.string_of_t p)

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

let query_cmd g cmd =
    begin
        match cmd with
        | "ports"::remain -> port_query g remain
        | "list"::remain -> list_query g remain
        | "biggest"::remain -> biggest_query g remain
        | "smallest"::remain -> smallest_query g remain
        | "average"::remain -> average_query g remain
        | other::_ -> printf "command error: query %s is not a valid query\n" other
        | [] -> printf "command error: no query given\n"
    end;
    g (* "return" the same graph *)

let edit_port_cmd g cmd =
    let aux code field remain = begin
        printf "setting %s to %s for %s\n" field remain code;
        let res = Graph.edit g (Graph.Edit.port_edit code field remain) in
        match (Graph.EditResult.new_graph res) with
        | None    -> (printf "error doing update\n"; g)
        | Some gg -> gg
    end in
    match cmd with
    | _::[]                   -> (printf "command error: edit code field new_val\n"; g)
    | code::field::remain::[] -> aux code field remain
    | _                       -> (printf "command error: edit code field new_val\n"; g)

let edit_routes_cmd g cmd = printf "edit a route\n"; g

let edit_cmd g cmd =
    match cmd with
    | "port"::remain -> edit_port_cmd g remain
    | "routes"::remain -> edit_routes_cmd g remain
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
        | filename::[] -> printf "TODO write data to %s\n" filename
        | _::_::_      -> printf "command error: I'm not sure what you are trying to do man\n"
        | []           -> printf "command error: write command needs a filename\n"
    end;
    g

let command g cmd =
    match cmd with
    | "query"::remain -> query_cmd g remain
    | "edit"::remain  -> edit_cmd g remain
    | "map"::remain   -> map_cmd g remain
    | "write"::remain -> write_cmd g remain
    | _ -> printf "error: command not recognized\n"; g (* in error, don't change anything *)

let () =
    let g = Graph.t_of_dataset (Map_data_j.dataset_of_string (In_channel.read_all "map_data.json")) in
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
