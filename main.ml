open Core.Std
open Map_data_t

let list_all_cities g =
    List.iter ~f:(fun p -> printf "%s\n" p.name) (Graph.all_ports g)

let port_info_for g code =
    let port = Graph.port_for_code g code in
    match port with
    | None   -> printf "port %s found!\n" code
    | Some p -> printf "port info for %s\n%s\n" code (Port.string_of_t p)

let print_stats g =
    begin
        printf "Longest Path: %d\n"        (Graph.longest_path g);
        printf "Shortest Path: %d\n"       (Graph.shortest_path g);
        printf "Largest Population: %d\n"  (Graph.largest_pop g);
        printf "Smallest Population: %d\n" (Graph.smallest_pop g);
        printf "Average Population: %f\n"  (Graph.average_population g);
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
        )
        (fun list_cities port_info stats () -> begin
            if list_cities                then (list_all_cities g);
            if (Option.is_some port_info) then (port_info_for g (Option.value_exn port_info));
            if stats                      then (print_stats g);
        end)

let () =
    let g = Graph.from (Map_data_j.dataset_of_string (In_channel.read_all "map_data.json")) in
    Command.run (command g)
