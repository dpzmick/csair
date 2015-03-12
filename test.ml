open Core.Std
open OUnit2

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

let generic_edit_fail ~dataset ~edit ~expected_error =
    let g = Graph.t_of_dataset dataset in
    let g_fail = Edit.apply_to g edit in
    match (Edit_result.new_graph g_fail) with
    | None   -> assert_equal (Edit_result.failure_reason g_fail) expected_error
    | Some _ -> assert_failure "edit should have failed"

let generic_edit_success ~dataset ~edit ~after =
    let g = Graph.t_of_dataset dataset in
    let g_success = Edit.apply_to g edit in
    match (Edit_result.new_graph g_success) with
    | None    -> assert_failure "should have edited successfully"
    | Some gg -> (after gg)

let test_port_add _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.port_add "ASD")
        ~after:(fun gg ->
            let shoulds = ["SCL"; "MEX"; "LIM"; "ASD"] in
            let actuals = List.map (Graph.all_ports gg) ~f:Port.code in
            assert_contains_all shoulds actuals)

let test_port_add_exists _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_add "SCL")
        ~expected_error:"port already exists"

(* try to add a route between a place and itself *)
let test_route_add_same_place _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"ASD" ~to_code:"ASD" ~dist:"100")
        ~expected_error:"source and dest are the same port"

let test_route_exists _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"MEX" ~to_code:"SCL" ~dist:"9982")
        ~expected_error:"this route already exists"

(* try to add a route with one of the places non-existent *)
let test_route_add_dne_src _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"ASD" ~to_code:"SCL" ~dist:"100")
        ~expected_error:"source doesn't exist"

let test_route_add_dne_dest _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"SCL" ~to_code:"ASD" ~dist:"100")
        ~expected_error:"dest doesn't exist"

(* NOTE: remember that mini_data is not directed *)
(* NOTE: but, the add route only adds one direction *)
(* should work *)
let test_route_add _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"LIM" ~to_code:"SCL" ~dist:"100")
        ~after:(fun gg ->
            let shoulds = [("SCL", 2453); ("MEX", 1235); ("SCL", 100)] in (* (code,distance) of reachable *)
            let actuals = (Graph.routes_from_port gg (Option.value_exn (Graph.port_for_code gg "LIM"))) in
            let actuals = List.map actuals ~f:(fun r -> (Port.code (Route.to_port r), Route.distance r)) in
            assert_contains_all actuals shoulds)

let test_route_add_non_int _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"SCL" ~to_code:"MEX" ~dist:"ASD")
        ~expected_error:"distance not an integer"

let test_route_add_negative _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"SCL" ~to_code:"MEX" ~dist:"-1")
        ~expected_error:"distance must be greater than 0"

let test_route_add_zero _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_add ~from_code:"SCL" ~to_code:"MEX" ~dist:"0")
        ~expected_error:"distance must be greater than 0"

let test_port_edit_string _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"name" ~value:"new name")
        ~after:(fun gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            assert_equal (Port.name p) "new name")

let test_port_edit_negative _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"population" ~value:"-1")
        ~expected_error:"number must be positive"

let test_port_edit_tz_err _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"not a float")
        ~expected_error:"need floating point number"

let test_port_edit_dne _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"DNE" ~field:"timezone" ~value:"1.0")
        ~expected_error:"port does not exist"

let test_port_edit_tz_succ1 _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"1.5")
        ~after:(fun gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            let tz = Port.timezone p in
            assert_equal tz 1.5)

let test_port_edit_tz_succ2 _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"timezone" ~value:"1")
        ~after:(fun gg ->
            let p = Option.value_exn (Graph.port_for_code gg "SCL") in
            let tz = Port.timezone p in
            assert_equal tz 1.0)

let test_port_edit_no_field _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_edit ~code:"SCL" ~field:"DNE" ~value:"1")
        ~expected_error:"field does not exist"

let test_route_edit_dne_start _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_edit ~from_code:"DNE" ~to_code:"SCL" ~new_dist:"100")
        ~expected_error:"start port does not exist"

let test_route_edit_dne_end _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_edit ~from_code:"SCL" ~to_code:"DNE" ~new_dist:"100")
        ~expected_error:"end port does not exist"

let test_route_edit_not_int _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_edit ~from_code:"SCL" ~to_code:"LIM" ~new_dist:"lolol")
        ~expected_error:"new distance must be an integer"

let test_route_edit_negative _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_edit ~from_code:"SCL" ~to_code:"LIM" ~new_dist:"-1")
        ~expected_error:"new distance must be positive"

(* NOTE: loading the directed data for this test *)
let test_route_edit_dne_route _ =
    generic_edit_fail
        ~dataset:directed_data
        ~edit:(Edit.route_edit ~from_code:"MEX" ~to_code:"LIM" ~new_dist:"100")
        ~expected_error:"route does not exist"

(* NOTE: loading the directed data for this test *)
let test_route_edit_success _ =
    generic_edit_success
        ~dataset:directed_data
        ~edit:(Edit.route_edit ~from_code:"SCL" ~to_code:"LIM" ~new_dist:"100")
        ~after:(fun gg ->
            let shoulds = [("SCL","LIM",100);("LIM","MEX",1235);("MEX","SCL",9982)] in
            let actuals = Graph.all_routes gg in
            let actuals = List.map actuals ~f:(fun r ->
                ((Port.code (Route.from_port r)), (Port.code (Route.to_port r)), (Route.distance r)))
            in
            assert_contains_all actuals shoulds)

let test_port_rm_dne _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.port_delete "DNE")
        ~expected_error:"port does not exist"

let test_port_rm_succ _ =
    generic_edit_success
        ~dataset:mini_data
        ~edit:(Edit.port_delete "SCL")
        ~after:(fun gg ->
            let shoulds_ports = ["LIM";"MEX"] in
            let actuals_ports = List.map (Graph.all_ports gg) ~f:Port.code in
            assert_contains_all shoulds_ports actuals_ports;
            List.iter (Graph.all_routes gg)
                ~f:(fun r ->
                    assert_equal false (String.equal (Port.code (Route.from_port r)) "SCL");
                    assert_equal false (String.equal (Port.code (Route.to_port   r)) "SCL")))

let test_route_rm_start_dne _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_delete ~from_code:"DNE" ~to_code:"SCL" ~dist:"")
        ~expected_error:"start port does not exist"

let test_route_rm_end_dne _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_delete ~from_code:"SCL" ~to_code:"DNE" ~dist:"")
        ~expected_error:"end port does not exist"

let test_route_rm_dne1 _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_delete ~from_code:"SCL" ~to_code:"MEX" ~dist:"10")
        ~expected_error:"route does not exist"

let test_route_rm_dne2 _ =
    generic_edit_fail
        ~dataset:directed_data
        ~edit:(Edit.route_delete ~from_code:"SCL" ~to_code:"MEX" ~dist:"")
        ~expected_error:"route does not exist"

let test_route_rm_nonint _ =
    generic_edit_fail
        ~dataset:mini_data
        ~edit:(Edit.route_delete ~from_code:"MEX" ~to_code:"SCL" ~dist:"1.5")
        ~expected_error:"distance not an integer"

(* cant use generic dude for this :( *)
let test_route_rm_multiple _ =
    let g = Graph.t_of_dataset mini_data in
    let e1 = Edit.apply_to g (Edit.route_add ~from_code:"SCL" ~to_code:"MEX" ~dist:"10") in
    match (Edit_result.new_graph e1) with
    | None    -> assert_failure "first edit should not have failed"
    | Some gg ->
            begin
                let e2 = Edit.apply_to gg (Edit.route_delete ~from_code:"SCL" ~to_code:"MEX" ~dist:"") in
                match (Edit_result.new_graph e2) with
                | None   -> assert_equal
                                "multiple routes exist, need to specify a distance"
                                (Edit_result.failure_reason e2)
                | Some _ -> assert_failure "edit should have failed"
            end;
            begin
                let e2 = Edit.apply_to gg (Edit.route_delete ~from_code:"SCL" ~to_code:"MEX" ~dist:"10") in
                match (Edit_result.new_graph e2) with
                | None   -> assert_failure "edit should have succeeded"
                | Some _ -> assert_equal true true (* just let this go, we test this elsewhere *)
            end

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
         "test_port_add_exists">::      test_port_add_exists;
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
         "test_port_edit_negative">::   test_port_edit_negative;
         "test_route_exists">::         test_route_exists;
         "test_route_edit_dne_start">:: test_route_edit_dne_start;
         "test_route_edit_dne_end">::   test_route_edit_dne_end;
         "test_route_edit_dne_route">:: test_route_edit_dne_route;
         "test_route_edit_not_int">::   test_route_edit_not_int;
         "test_route_edit_negative">::  test_route_edit_negative;
         "test_route_edit_success">::   test_route_edit_success;
         "test_port_rm_dne">::          test_port_rm_dne;
         "test_port_rm_succ">::         test_port_rm_succ;
         "test_routes_rm_start_dne">::  test_route_rm_start_dne;
         "test_routes_rm_end_dne">::    test_route_rm_end_dne;
         "test_route_rm_dne1">::        test_route_rm_dne1;
         "test_route_rm_dne2">::        test_route_rm_dne2;
         "test_route_rm_nonint">::      test_route_rm_nonint;
         "test_route_rm_multiple">::    test_route_rm_multiple]

let () =
    run_test_tt_main suite
