open Core.Std

module T = struct
    type t = {
      n: int option;
      s: int option;
      e: int option;
      w: int option
    } with sexp, compare, fields
end

include T
include Comparable.Make(T)

let create ?(n = None) ?(s = None) ?(e = None) ?(w = None) () = {n;s;e;w}
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

