open Map_data_t
open Core.Std

type t = Map_data_t.coordinate

include Comparable.S with type t := t

val string_of_t : t -> string
val empty : t
val create : ?n:(int option) -> ?s:(int option) -> ?e:(int option) -> ?w:(int option) -> unit -> t
