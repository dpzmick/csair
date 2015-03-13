open Graph

type t

val t_of_code_list : string list -> on:Graph.t -> t option

val valid_on_graph : t -> on:Graph.t -> bool

val cost_on_graph : t -> on:Graph.t -> int
val distance_on_graph : t -> on:Graph.t -> int
val time_to_travel_on_graph : t -> on:Graph.t -> int
