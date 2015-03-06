open Sexplib.Std
open Map_data_t
open Core.Std


module T = struct
    type t = Map_data_t.port
    let empty = {
        code = "";
        name = "";
        country = "";
        continent = "";
        timezone = 1.0;
        coordinates = Coordinates.empty;
        population = 1;
        region = 1;
    }

    let compare p1 p2 = String.compare p1.name p2.name

    (* TODO these are massive hacks!! *)
    let sexp_of_t p = String.sexp_of_t p.name
    let t_of_sexp s = empty
end

include T
include Comparable.Make(T)

let code       p = p.code
let name       p = p.name
let population p = p.population
let continent  p = p.continent

let string_of_t p =
    sprintf "code: %s\nname: %s\ncountry: %s\ncontinent: %s\ntimezone: %f\ncoords: %s\npopulation: %d\nregion: %d"
        p.code
        p.name
        p.country
        p.continent
        p.timezone
        (Coordinates.string_of_t p.coordinates)
        p.population
        p.region
