open Core.Std
open OUnit2
open Graph
include Map_data_t

let mini_data =
    let contents = In_channel.read_all "mini_data.json" in
    Map_data_j.dataset_of_string contents

let assert_contains_all shoulds actuals =
    List.iter ~f:(fun s -> assert_equal true (List.exists actuals ~f:(fun a -> a = s))) shoulds

let always_pass test_ctxt =
    assert_equal true true

(* check if all of the elements in the dataset are in the generated list of ports *)
let test_all_ports test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    let shoulds = mini_data.metros in
    let actuals = Graph.all_ports g in
    assert_contains_all shoulds actuals

(* check that if I query a port by three letter value I get what I wanted *)
let test_port_for_code test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    let p = Graph.port_for_code g "SCL" in
    match p with
    | None   -> assert_failure "the port wasn't found"
    | Some p -> begin
        assert_equal p.code          "SCL";
        assert_equal p.name          "Santiago";
        assert_equal p.country       "CL";
        assert_equal p.continent     "South America";
        assert_equal p.timezone      (-4.0);
        assert_equal p.coordinates.s (Some 33);
        assert_equal p.coordinates.w (Some 71);
        assert_equal p.population    6000000;
        assert_equal p.region        1;
    end

(* Test to see if I can get everywhere you can fly from this city *)
let test_routes_from_port test_ctxt =
    let should = ["LIM"; "MEX"] in
    let g = Graph.t_of_dataset mini_data in
    let p = Graph.port_for_code g "SCL" in
    match p with
    | None    -> assert_failure "the port wasn't found"
    | Some p ->
            let outgoing = Graph.routes_from_port g p in
            let ports = List.map ~f:Route.to_port outgoing in
            let codes = List.map ~f:Port.code ports in
            assert_contains_all should codes

let test_longest test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    match (Graph.longest_path g) with
    | None    -> assert_failure "Not found"
    | Some r -> assert_equal (Route.distance r) 9982

let test_shortest test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    match (Graph.shortest_path g) with
    | None    -> assert_failure "Not found"
    | Some r -> assert_equal (Route.distance r) 1235

let test_pop_stats test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    begin
        assert_equal 6000000 (Graph.smallest_pop g);
        assert_equal 23400000 (Graph.largest_pop g);
    end

let test_average_pop test_ctxt =
    let g = Graph.t_of_dataset mini_data in
    assert_equal (cmp_float (Graph.average_population g) 12816666.6667) true

let test_continent_thing test_ctxt =
    let aux cont shoulds actuals =
        let names = List.Assoc.find_exn shoulds cont in
        match List.Assoc.find actuals cont with
        | None -> assert_failure "Something is wrong"
        | Some onames -> assert_contains_all names onames
    in

    let shoulds = [("North America", ["Mexico City"]); ("South America", ["Lima"; "Santiago"])] in
    let g = Graph.t_of_dataset mini_data in
    let actuals = Graph.continents_served g in
    begin
        aux "North America" shoulds actuals;
        aux "South America" shoulds actuals;
    end

let test_hubs test_ctxt =
    let shoulds = ["SCL";"LIM";"MEX"] in (* TODO generate better test data *)
    let g = Graph.t_of_dataset mini_data in
    let hubs = Graph.hubs g in
    match hubs with
    | None -> assert_failure "no hubs found"
    | Some (d, ports) -> assert_contains_all shoulds (List.map ports ~f:Port.code)

(* NOTE: adding both directions to get the initial heading and stuff for both *)
let test_gcm test_ctxt =
    let should = "http://www.gcmap.com/mapui?P=SCL-MEX%2C+SCL-LIM%2C+MEX-SCL%2C+MEX-LIM%2C+LIM-MEX%2C+LIM-SCL&MS=wls&DU=mi" in
    let g = Graph.t_of_dataset mini_data in
    let gcm = Gcm_data.from g in
    let gcm_res = Gcm_data.string_of_t gcm in
    assert_equal should gcm_res

let suite =
    "suite">:::
        ["always_pass">::          always_pass;
         "all_ports">::            test_all_ports;
         "port_for_code">::        test_port_for_code;
         "routes_from_port">::     test_routes_from_port;
         "test_longest">::         test_longest;
         "test_shortest">::        test_shortest;
         "test_pop_stats">::       test_pop_stats;
         "test_average_pop">::     test_average_pop;
         "test_continent_thing">:: test_continent_thing;
         "test_hubs">::            test_hubs;
         "test_gcm">::             test_gcm]

let () =
    run_test_tt_main suite
