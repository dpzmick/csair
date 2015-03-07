open Core.Std
open Map_data_t
open Sys

let list_all_cities g =
    List.iter ~f:(fun p -> printf "%s\n" p.name) (Graph.all_ports g)

let port_info_for g code =
    let port = Graph.port_for_code g code in
    match port with
    | None   -> printf "port %s not found!\n" code
    | Some p -> printf "port info for %s\n---\n%s\n" code (Port.string_of_t p)

let port_query g cmd =
    match cmd with
    | codes -> List.iter codes ~f:(port_info_for g)
    | [] -> printf "command error: port codes not specified"

let longest_single_flight_cmd g =
    let longest = Graph.longest_path g in
    match longest with
    | None -> printf "there was no longest flight\n"
    | Some r -> printf "Longest flight: %s -> %s, distance: %d\n"
                    (Port.code (Route.from_port r)) (Port.code (Route.to_port r)) (Route.distance r)

let greatest_query g cmd =
    match cmd with
    | "flight"::[] -> longest_single_flight_cmd g
    | "population"::[] -> printf "Largest Population: %d\n"  (Graph.largest_pop g);
    | _ -> printf "command error: error to greatest query\n"

let smallest_single_flight_cmd g =
    let longest = Graph.shortest_path g in
    match longest with
    | None -> printf "there was no shortest flight\n"
    | Some r -> printf "Shortest flight: %s -> %s, distance: %d\n"
                    (Port.code (Route.from_port r)) (Port.code (Route.to_port r)) (Route.distance r)

let smallest_query g cmd =
    match cmd with
    | "flight"::[] -> smallest_single_flight_cmd g
    | "population"::[] -> printf "Smallest Population: %d\n"  (Graph.smallest_pop g);
    | _ -> printf "command error: error to greatest query\n"

let average_query g cmd =
    match cmd with
    | "flight"::[] -> printf "not yet implemented\n"
    | "population"::[] -> printf "Average Population: %f\n"  (Graph.average_population g);
    | _ -> printf "command error: error to greatest query\n"

let size_query g cmd =
    match cmd with
    | "greatest"::remain -> greatest_query g remain
    | "smallest"::remain -> smallest_query g remain
    | "average"::remain -> average_query g remain
    | other::_ -> printf "command error: invalid size query %s\n" other
    | [] -> printf "command error: no size subquery given\n"

let print_continents g =
    List.iter (Graph.continents_served g)
        ~f:(fun (continent, cities) -> begin
            printf "Cities reachable on continent %s:\n" continent;
            List.iter cities
                ~f:(fun city -> printf "\t%s\n" city)
    end)

let print_hubs g =
    match (Graph.hubs g) with
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

let info_query g cmd =
    match cmd with
    | "size"::remain -> size_query g remain
    | "list"::remain -> list_query g remain
    | other::_ -> printf "command error: invalid info query %s\n" other
    | [] -> printf "command error: no info subquery given\n"

let query_cmd g cmd =
    match cmd with
    | "ports"::remain -> port_query g remain
    | "info"::remain -> info_query g remain
    | other::_ -> printf "command error: query %s is not a valid query\n" other
    | [] -> printf "command error: no query given\n"

let edit_cmd g cmd = printf "edit cmd\n"

let command g cmd =
    match cmd with
    | _::"query"::remain -> query_cmd g remain
    | _::"edit"::remain  -> edit_cmd  g remain
    | _ -> printf "error: command not recognized\n"

let () =
    let g = Graph.t_of_dataset (Map_data_j.dataset_of_string (In_channel.read_all "map_data.json")) in
    command g (Array.to_list Sys.argv)
