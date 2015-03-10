val longest_path       : Graph.t -> Route.t option
val shortest_path      : Graph.t -> Route.t option
val largest_pop        : Graph.t -> int
val smallest_pop       : Graph.t -> int
val average_population : Graph.t -> float
val continents_served  : Graph.t -> (string * (string list)) list
val hubs               : Graph.t -> (int * Port.t list) option
