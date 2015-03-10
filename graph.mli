open Core.Std

module Edit : sig
    type t
    val port : string -> string -> string -> t
end

module EditResult : sig
    type 'a t
    val create : 'a option -> 'a t
    val new_graph : 'a t -> 'a option
end

type t

val empty : unit -> t
val t_of_dataset : Map_data_t.dataset -> t

val port_for_code : t -> string -> Port.t option
val routes_from_port: t -> Port.t -> Route.t list
val all_ports : t -> Port.t list
val all_routes : t -> Route.t list

val edit : t -> Edit.t -> t EditResult.t
