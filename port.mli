open Core.Std
open Sexplib.Std

type t = {
  code: string;
  name: string;
  country: string;
  continent: string;
  timezone: float;
  coordinates: Coordinates.t;
  population: int;
  region: int
} with sexp, compare, fields

include Comparable.S with type t := t

val default_of_code : string -> t
val modify_old : t -> field:string -> value:string -> t

val string_of_t : t -> string
