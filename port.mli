open Map_data_t
open Core.Std

type t = Map_data_t.port

include Comparable.S with type t := t

val string_of_t : t -> string
val empty : t

val code : t -> string
