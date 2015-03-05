open Core.Std

type t

val empty : unit -> t
val from : Map_data_t.dataset -> t
val all_ports : t -> Map_data_t.port list
val port_info_for : t -> string -> Map_data_t.port option
