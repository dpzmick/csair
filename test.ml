open Core.Std
open OUnit2
open Graph
include Map_data_t

let mini_data =
    let contents = In_channel.read_all "mini_data.json" in
    Map_data_j.dataset_of_string contents

let assert_contains_all shoulds actuals =
    List.iter ~f:(fun s -> assert_equal true (List.exists actuals (fun a -> a = s))) shoulds

let always_pass test_ctxt =
    assert_equal true true

(* check if all of the elements in the dataset are in the generated list of ports *)
let test_all_ports test_ctxt =
    let g = Graph.from mini_data in
    let shoulds = mini_data.metros in
    let actuals = Graph.all_ports g in
    assert_contains_all shoulds actuals

(* check that if I query a port by three letter value I get what I wanted *)
let test_port_info test_ctxt =
    let g = Graph.from mini_data in
    let p = Graph.port_info_for g "SCL" in
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

let suite =
    "suite">:::
        ["always_pass">:: always_pass;
         "all_ports">::   test_all_ports;
         "port_info">::   test_port_info]

let () =
    run_test_tt_main suite
