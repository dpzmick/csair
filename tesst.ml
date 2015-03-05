open Core.Std
open Graph
open Map_data_t

let () =
    let contents = In_channel.read_all "map_data.json" in
    let data = Map_data_j.dataset_of_string contents in
    let g = Graph.from data in
    let ps = Graph.all_ports g in
    let pp = Graph.port_info_for g "SCL" in
    (* List.iter ~f:(fun e -> printf "%s\n" e.name) ps *)
    match pp with
    | None -> printf "didnt work"
    | Some p -> printf "%s\n" p.name
