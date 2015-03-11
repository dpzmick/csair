open Map_data_t
open Core.Std

type t = Map_data_t.coordinate

let empty = {n=None;s=None;e=None;w=None}
let create ?(n = None) ?(s = None) ?(e = None) ?(w = None) () = {n;s;e;w}

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

let equal c1 c2 =
    let l = (Option.equal Int.equal c1.n c2.n)::[] in
    let l = (Option.equal Int.equal c1.s c2.s)::l in
    let l = (Option.equal Int.equal c1.e c2.e)::l in
    let l = (Option.equal Int.equal c1.w c2.w)::l in
    List.fold l ~init:true ~f:(&&)

