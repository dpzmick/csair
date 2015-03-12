open Core.Std

type t

val empty : unit -> t
val t_of_dataset : Map_data_t.dataset -> t
val dataset_of_t : t -> Map_data_t.dataset

val port_for_code : t -> string -> Port.t option
val routes_from_port: t -> Port.t -> Route.t list

val add_all_routes : t -> Map_data_t.json_route list -> bool -> t
val add_all_ports : t -> Port.t list -> t

val all_ports : t -> Port.t list
val all_routes : t -> Route.t list

val routes_from : t -> Port.t -> Route.t list option
val routes_from_exn : t -> Port.t -> Route.t list

val set_routes_from : t -> port:Port.t -> routes:Route.t list -> t

val without_port_no_cleanup : t -> Port.t -> t
