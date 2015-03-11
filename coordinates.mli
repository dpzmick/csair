open Map_data_t

type t = Map_data_t.coordinate

val string_of_t : t -> string
val empty : t
val create : ?n:(int option) -> ?s:(int option) -> ?e:(int option) -> ?w:(int option) -> unit -> t

val equal : t -> t -> bool
