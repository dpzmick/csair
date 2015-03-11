type t

(** creates a new t with from port, to port, and distance *)
val create : Port.t -> Port.t -> int -> t

val to_json_route : t -> Map_data_t.json_route
val from_port : t -> Port.t
val to_port : t -> Port.t
val distance : t -> int

val equal : t -> t -> bool
