type 'a t

val create : 'a -> 'a t
val fail : string -> 'a t

val new_graph : 'a t -> 'a option
val failure_reason : 'a t -> string
