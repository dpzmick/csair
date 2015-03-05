open Core.Std
open OUnit2
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

let suite =
    "suite">:::
        ["always_pass">:: always_pass;
         "all_ports">::   test_all_ports]

let () =
    run_test_tt_main suite
