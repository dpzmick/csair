open Map_data_t
open Core.Std

type t = Map_data_t.port

include Comparable.S with type t := t

val default_of_code : string -> t

val string_of_t : t -> string

val code : t -> string
val name : t -> string
val population : t -> int
val timezone : t -> float
val continent : t -> string
