open Core.Std
open OUnit2
open Graph
open Graph_analytics
include Map_data_t

let mini_data =
    let contents = In_channel.read_all "mini_data.json" in
    Map_data_j.dataset_of_string contents

let directed_data =
    let contents = In_channel.read_all "directed_data.json" in
    Map_data_j.dataset_of_string contents

let assert_contains_all shoulds actuals =
    begin
        List.iter ~f:(fun s -> assert_equal true (List.exists actuals ~f:(fun a -> a = s))) shoulds;
        List.iter ~f:(fun s -> assert_equal true (List.exists shoulds ~f:(fun a -> a = s))) actuals;
    end

let always_pass _ =
    assert_equal true true

let test_coord_equal _ =
    let c1 = Coordinates.create ~n:(Some 10) ~e:(Some 10) () in
    let c2 = Coordinates.create ~n:(Some 10) ~e:(Some 10) () in
    assert_equal true (Coordinates.equal c1 c2)

let test_port_equal _ =
    let p1 = Port.default_of_code "ASD" in
    let p2 = Port.default_of_code "ASD" in
    assert_equal true (Port.equal p1 p2)

let test_route_equal _ =
    let p1 = Port.default_of_code "ASD" in
    let p2 = Port.default_of_code "DSA" in
    let r1 = Route.create p1 p2 10 in
    let r2 = Route.create p1 p2 10 in
    assert_equal true (Route.equal r1 r2)

(* check if all of the elements in the dataset are in the generated list of ports *)
let test_all_ports _ =
    let g = Graph.t_of_dataset mini_data in
    let shoulds = mini_data.metros in
    let actuals = Graph.all_ports g in
    assert_contains_all shoulds actuals

(* check that if I query a port by three letter value I get what I wanted *)
let test_port_for_code _ =
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
let test_routes_from_port _ =
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

(* Test to see if I can get everywhere you can fly from this city (directed case) *)
let test_routes_from_port_directed _ =
    let should = ["LIM"] in
    let g = Graph.t_of_dataset directed_data in
    let p = Graph.port_for_code g "SCL" in
    match p with
    | None    -> assert_failure "the port wasn't found"
    | Some p ->
            let outgoing = Graph.routes_from_port g p in
            let ports = List.map ~f:Route.to_port outgoing in
            let codes = List.map ~f:Port.code ports in
            assert_contains_all should codes

let test_longest _ =
    let g = Graph.t_of_dataset mini_data in
    match (Graph_analytics.longest_path g) with
    | None    -> assert_failure "Not found"
    | Some r -> assert_equal (Route.distance r) 9982

let test_shortest _ =
    let g = Graph.t_of_dataset mini_data in
    match (Graph_analytics.shortest_path g) with
    | None    -> assert_failure "Not found"
    | Some r -> assert_equal (Route.distance r) 1235

let test_pop_stats _ =
    let g = Graph.t_of_dataset mini_data in
    begin
        assert_equal 6000000 (Graph_analytics.smallest_pop g);
        assert_equal 23400000 (Graph_analytics.largest_pop g);
    end

let test_average_pop _ =
    let g = Graph.t_of_dataset mini_data in
    assert_equal (cmp_float (Graph_analytics.average_population g) 12816666.6667) true

let test_continent_thing _ =
    let aux cont shoulds actuals =
        let names = List.Assoc.find_exn shoulds cont in
        match List.Assoc.find actuals cont with
        | None -> assert_failure "Something is wrong"
        | Some onames -> assert_contains_all names onames
    in

    let shoulds = [("North America", ["Mexico City"]); ("South America", ["Lima"; "Santiago"])] in
    let g = Graph.t_of_dataset mini_data in
    let actuals = Graph_analytics.continents_served g in
    begin
        aux "North America" shoulds actuals;
        aux "South America" shoulds actuals;
    end

let test_hubs _ =
    let shoulds = ["SCL";"LIM";"MEX"] in (* TODO generate better test data *)
    let g = Graph.t_of_dataset mini_data in
    let hubs = Graph_analytics.hubs g in
    match hubs with
    | None -> assert_failure "no hubs found"
    | Some (_, ports) -> assert_contains_all shoulds (List.map ports ~f:Port.code)

let test_port_add _ =
    let g = Graph.t_of_dataset mini_data in
    (* try to add port with existing code *)
    begin
        let g_fail = Graph.edit g (Graph.Edit.port_add "SCL") in
        match (Graph.EditResult.new_graph g_fail) with
        | None   -> assert_equal true true
        | Some _ -> assert_failure "should have failed"
    end;
    begin
        let g_success = Graph.edit g (Graph.Edit.port_add "ASD") in
        match (Graph.EditResult.new_graph g_success) with
        | None    -> assert_failure "should have added successfully"
        | Some gg ->
                let shoulds = ["SCL"; "MEX"; "LIM"; "ASD"] in
                let actuals = List.map (Graph.all_ports gg) ~f:Port.code in
                assert_contains_all shoulds actuals
    end

(* try to add a route between a place and itself *)
let test_route_add_same_place _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "ASD" "ASD" "100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal (Graph.EditResult.failure_reason g_fail) "source and dest are the same port"
    | Some _ -> assert_failure "Should not have been able to add the route"

(* try to add a route with one of the places non-existent *)
let test_route_add_dne_src _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "ASD" "SCL" "100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal (Graph.EditResult.failure_reason g_fail) "source doesn't exist"
    | Some _ -> assert_failure "Should not have been able to add the route"

let test_route_add_dne_dest _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "SCL" "ASD" "100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal (Graph.EditResult.failure_reason g_fail) "dest doesn't exist"
    | Some _ -> assert_failure "Should not have been able to add the route"

(* should work *)
let test_route_add _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "LIM" "SCL" "100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_failure "should not have failed"
    | Some gg ->
            (* NOTE: remember that mini_data is not directed *)
            (* NOTE: but, the add route only adds one direction *)
            let shoulds = [("SCL", 2453); ("MEX", 1235); ("SCL", 100)] in (* (code,distance) of reachable *)
            let actuals = (Graph.routes_from_port gg (Option.value_exn (Graph.port_for_code gg "LIM"))) in
            let actuals = List.map actuals ~f:(fun r -> (Port.code (Route.to_port r), Route.distance r)) in
            assert_contains_all actuals shoulds

let test_route_add_non_int _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "SCL" "MEX" "ASD") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_equal (Graph.EditResult.failure_reason g_fail) "distance not a number"
    | Some _ -> assert_failure "should have failed"

let test_route_add_negative _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "SCL" "MEX" "-1") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_equal (Graph.EditResult.failure_reason g_fail) "distance must be greater than 0"
    | Some _ -> assert_failure "should have failed"

let test_route_add_zero _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_add "SCL" "MEX" "0") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_equal (Graph.EditResult.failure_reason g_fail) "distance must be greater than 0"
    | Some _ -> assert_failure "should have failed"

let test_port_edit_string _ =
    let g = Graph.t_of_dataset mini_data in
    let g_success = Graph.edit g (Graph.Edit.port_edit ~code:"SCL" ~field:"name" ~value:"new name") in
    match (Graph.EditResult.new_graph g_success) with
    | None    -> assert_failure "should have succeeded"
    | Some gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            assert_equal (Port.name p) "new name"

let test_port_edit_tz_err _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"not a float") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_equal "need floating point number" (Graph.EditResult.failure_reason g_fail)
    | Some _  -> assert_failure "should have failed to edit"

let test_port_edit_dne _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.port_edit ~code:"DNE" ~field:"timezone" ~value:"1.0") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_equal "port does not exist" (Graph.EditResult.failure_reason g_fail)
    | Some _  -> assert_failure "should have failed to edit"

let test_port_edit_tz_succ1 _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"1.5") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_failure "should not have failed to edit"
    | Some gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            let tz = Port.timezone p in
            assert_equal tz 1.5

let test_port_edit_tz_succ2 _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"1") in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_failure "should not have failed to edit"
    | Some gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            let tz = Port.timezone p in
            assert_equal tz 1.0

let test_port_edit_no_field _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.port_edit ~code:"SCL" ~field:"DNE" ~value:"1") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal "field does not exist" (Graph.EditResult.failure_reason g_fail)
    | Some _ -> assert_failure "should have failed to edit"

let test_route_edit_dne_start _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_edit ~from_code:"DNE" ~to_code:"SCL" ~new_dist:"100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal "start does not exist" (Graph.EditResult.failure_reason g_fail)
    | Some _ -> assert_failure "should have failed to edit"

let test_route_edit_dne_end _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_edit ~from_code:"SCL" ~to_code:"DNE" ~new_dist:"100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal "end does not exist" (Graph.EditResult.failure_reason g_fail)
    | Some _ -> assert_failure "should have failed to edit"

let test_route_edit_not_int _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = Graph.edit g (Graph.Edit.route_edit ~from_code:"SCL" ~to_code:"LIM" ~new_dist:"lolol") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal "new distance must be an integer" (Graph.EditResult.failure_reason g_fail)
    | Some _ -> assert_failure "should have failed to edit"

let test_route_edit_dne_route _ =
    (* NOTE: loading the directed data for this test *)
    let g = Graph.t_of_dataset directed_data in
    let g_fail = Graph.edit g (Graph.Edit.route_edit ~from_code:"MEX" ~to_code:"LIM" ~new_dist:"100") in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal "route does not exist" (Graph.EditResult.failure_reason g_fail)
    | Some _ -> assert_failure "should have failed to edit"

let test_route_edit_success _ =
    (* NOTE: loading the directed data for this test *)
    let g = Graph.t_of_dataset directed_data in
    let g_success = Graph.edit g (Graph.Edit.route_edit ~from_code:"SCL" ~to_code:"LIM" ~new_dist:"100") in
    match (Graph.EditResult.new_graph g_success) with
    | None    -> assert_failure "should not have failed to edit"
    | Some gg ->
            let shoulds = [("SCL","LIM",100);("LIM","MEX",1235);("MEX","SCL",9982)] in
            let actuals = Graph.all_routes gg in
            let actuals = List.map actuals ~f:(fun r ->
                ((Port.code (Route.from_port r)), (Port.code (Route.to_port r)), (Route.distance r)))
            in
            assert_contains_all actuals shoulds

let test_port_rm_dne _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = (Graph.edit g (Graph.Edit.port_delete "DNE")) in
    match (Graph.EditResult.new_graph g_fail) with
    | None   -> assert_equal (Graph.EditResult.failure_reason g_fail) "port does not exist"
    | Some _ -> assert_failure "should have failed"

let test_port_rm_succ _ =
    let g = Graph.t_of_dataset mini_data in
    let g_fail = (Graph.edit g (Graph.Edit.port_delete "SCL")) in
    match (Graph.EditResult.new_graph g_fail) with
    | None    -> assert_failure "should not have failed"
    | Some gg ->
            let shoulds_ports = ["LIM";"MEX"] in
            let actuals_ports = List.map (Graph.all_ports gg) ~f:Port.code in
            assert_contains_all shoulds_ports actuals_ports;
            List.iter (Graph.all_routes gg)
                ~f:(fun r ->
                    assert_equal false (String.equal (Port.code (Route.from_port r)) "SCL");
                    assert_equal false (String.equal (Port.code (Route.to_port   r)) "SCL"))

let suite =
    "suite">:::
        ["always_pass">::               always_pass;
         "test_coord_equal">::          test_coord_equal;
         "test_port_equal">::           test_port_equal;
         "test_route_equal">::          test_route_equal;
         "all_ports">::                 test_all_ports;
         "port_for_code">::             test_port_for_code;
         "routes_from_port">::          test_routes_from_port;
         "routes_from_port_directed">:: test_routes_from_port_directed;
         "test_longest">::              test_longest;
         "test_shortest">::             test_shortest;
         "test_pop_stats">::            test_pop_stats;
         "test_average_pop">::          test_average_pop;
         "test_continent_thing">::      test_continent_thing;
         "test_hubs">::                 test_hubs;
         "test_port_add">::             test_port_add;
         "test_route_add_same_place">:: test_route_add_same_place;
         "test_route_add_dne_src">::    test_route_add_dne_src;
         "test_route_add_dne_dest">::   test_route_add_dne_dest;
         "test_route_add">::            test_route_add;
         "test_route_add_non_int">::    test_route_add_non_int;
         "test_route_add_negative">::   test_route_add_negative;
         "test_route_add_zero">::       test_route_add_zero;
         "test_port_edit_string">::     test_port_edit_string;
         "test_port_edit_tz_err">::     test_port_edit_tz_err;
         "test_port_edit_tz_succ1">::   test_port_edit_tz_succ1;
         "test_port_edit_tz_succ2">::   test_port_edit_tz_succ2;
         "test_port_edit_no_field">::   test_port_edit_no_field;
         "test_route_edit_dne_start">:: test_route_edit_dne_start;
         "test_route_edit_dne_end">::   test_route_edit_dne_end;
         "test_route_edit_dne_route">:: test_route_edit_dne_route;
         "test_route_edit_not_int">::   test_route_edit_not_int;
         "test_route_edit_success">::   test_route_edit_success;
         "test_port_rm_dne">::          test_port_rm_dne;
         "test_port_rm_succ">::         test_port_rm_succ]

let () =
    run_test_tt_main suite
