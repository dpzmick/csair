open Map_data_t
open Core.Std
open Sexplib.Std

type t = Map_data_t.port

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t

include Comparable.S with type t := t

val default_of_code : string -> t
val modify_old : t -> field:string -> value:string -> t

val string_of_t : t -> string

val code : t -> string
val name : t -> string
val population : t -> int
val timezone : t -> float
val continent : t -> string
