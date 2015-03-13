include Map_data_t
open Core.Std
open Sexplib.Std

module T = struct
    type t = {
        from_port: Port.t;
        to_port:   Port.t;
        distance:  int;
    } with sexp

    let compare r1 r2 =
        let ports = (Port.equal r1.from_port r2.from_port) && (Port.equal r1.to_port r2.to_port) in
        let dists = r1.distance = r2.distance in
        if (ports && dists)
        then Int.compare r1.distance r2.distance
        else -1
end

include T
include Comparable.Make(T)

let create from_port to_port distance = {from_port;to_port;distance}

let to_json_route p = {
    ports = [(Port.code p.from_port); (Port.code p.to_port)];
    distance = p.distance;
}

let from_port p = p.from_port
let to_port   p = p.to_port
let distance  p = p.distance
