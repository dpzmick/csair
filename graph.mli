open Core.Std

type t

val empty : unit -> t
val from : Map_data_t.dataset -> t
val all_ports : t -> Port.t list
val port_for_code : t -> string -> Port.t option
val routes_from_port: t -> Port.t -> (Port.t * int) list
val all_routes : t -> (Port.t * (Port.t * int) list) list

val longest_path : t -> int
val shortest_path: t -> int
val largest_pop: t -> int
val smallest_pop: t -> int
