open Core.Std
open Map_data_t
open Sys

let list_all_cities g =
    List.iter ~f:(fun p -> printf "%s\n" p.name) (Graph.all_ports g)

let port_info_for g code =
    let port = Graph.port_for_code g code in
    match port with
    | None   -> printf "port %s found!\n" code
    | Some p -> printf "port info for %s\n%s\n" code (Port.string_of_t p)

let print_stats g =
    let longest = Graph.longest_path g in
    let shortest = Graph.shortest_path g in
    begin
        match longest with
        | None -> printf "There was no longest path"
        | Some r -> printf "Longest Path: %d\n" (Route.distance r);

        match shortest with
        | None -> printf "There was no shortest path"
        | Some r -> printf "Shortest Path: %d\n" (Route.distance r);

        printf "Largest Population: %d\n"  (Graph.largest_pop g);
        printf "Smallest Population: %d\n" (Graph.smallest_pop g);
        printf "Average Population: %f\n"  (Graph.average_population g);
        List.iter (Graph.continents_served g)
            ~f:(fun (continent, cities) -> begin
                printf "Cities reachable on continent %s:\n" continent;
                List.iter cities
                    ~f:(fun city -> printf "\t%s\n" city)
            end);
        match (Graph.hubs g) with
        | None -> printf "No hubs\n"
        | Some (hub_connections, hubs) -> begin
                printf "Hubs with %d connections: " hub_connections;
                List.iter hubs ~f:(fun hub -> printf "%s, " (Port.name hub));
                printf "\n";
        end
    end

let command g =
    Command.basic
        ~summary: "Query the flight database"
        ~readme:(fun () -> "halp")
        Command.Spec.(
            empty
            +> flag "-L" no_arg ~doc: " list all the cities CSAir flies to"
            +> flag "-P" (optional string) ~doc:"code get information about a specific port (given by port code)"
            +> flag "-S" no_arg ~doc:" get statistical information about the CSAir network"
            +> flag "-M" no_arg ~doc:" open great circle mapper with this map"
        )
        (fun list_cities port_info stats gcm () -> begin
            if list_cities                then (list_all_cities g);
            if (Option.is_some port_info) then (port_info_for g (Option.value_exn port_info));
            if stats                      then (print_stats g);
            if gcm                        then printf "return: %d\n"
                                                (Sys.command (sprintf "open %s" (Gcm_data.string_of_t (Gcm_data.from g))));
        end)

let () =
    let g = Graph.t_of_dataset (Map_data_j.dataset_of_string (In_channel.read_all "map_data.json")) in
    Command.run (command g)
