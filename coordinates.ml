open Map_data_t
open Core.Std

type t = Map_data_t.coordinate

let empty = {n=None;s=None;e=None;w=None}

let string_of_t coord =
    let s1 = match coord.n with
             | None   -> ""
             | Some x -> sprintf "n: %d, " x
    in
    let s2 = match coord.s with
             | None -> s1
             | Some x -> sprintf "%ss: %d, " s1 x
    in
    let s3 = match coord.e with
             | None -> s2
             | Some x -> sprintf "%se: %d, " s2 x
    in
    match coord.w with
    | None -> s3
    | Some x -> sprintf "%sw: %d" s3 x
