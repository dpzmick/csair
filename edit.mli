open Graph
open Edit_result

type t

val port_edit : code:string -> field:string -> value:string -> t
val port_delete : string -> t
val port_add : string -> t

val route_edit : from_code:string -> to_code:string -> new_dist:string -> t
val route_delete : from_code:string -> to_code:string -> dist:string -> t
val route_add : from_code:string -> to_code:string -> dist:string -> t

val apply_to : Graph.t -> t -> Graph.t Edit_result.t
