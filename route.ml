include Map_data_t

type t = {
    from_port: Port.t;
    to_port:   Port.t;
    distance:  int;
}

let create from_port to_port distance = {from_port;to_port;distance}

let to_json_route p = {
    ports = [(Port.code p.from_port); (Port.code p.to_port)];
    distance = p.distance;
}

let from_port p = p.from_port
let to_port p = p.to_port
let distance p = p.distance