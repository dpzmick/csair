open Core.Std

module Edit : sig
    type t
end

module EditResult : sig
    type t
end

type t

val empty : unit -> t
val t_of_dataset : Map_data_t.dataset -> t

val port_for_code : t -> string -> Port.t option
val routes_from_port: t -> Port.t -> Route.t list
val all_ports : t -> Port.t list
val all_routes : t -> Route.t list

val edit : t -> Edit.t -> EditResult.t
