open Core.Std
open Map_data_t

let list_all_cities g =
    List.iter ~f:(fun p -> printf "%s\n" p.name) (Graph.all_ports g)

let port_info_for g p =
    let port = Graph.port_for_code g p in
    match port with
    | None   -> printf "port %s found!\n" p
    | Some p -> printf "port info: name: %s\n" p.name

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
            if stats                      then printf "stats";
        end)

let () =
    let g = Graph.from (Map_data_j.dataset_of_string (In_channel.read_all "map_data.json")) in
    Command.run (command g)
