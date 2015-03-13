open Core.Std

module T = struct
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
end

include T
include Comparable.Make(T)

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

let default_of_code code = {empty with code = code}

let population_update p value =
    let v = int_of_string value in
    if (Int.(<) v 0)
    then raise (Invalid_argument "negative")
    else {p with population = v}

let modify_old p ~field ~value =
    match field with
    | "code"       -> {p with code = value}
    | "name"       -> {p with name = value}
    | "country"    -> {p with country = value}
    | "continent"  -> {p with continent = value}
    | "timezone"   -> (try {p with timezone = (Float.of_string value)} with
                      | Invalid_argument _ -> raise (Invalid_argument "float"))
    | "population" -> population_update p value
    | "region"     -> {p with region = (int_of_string value)}
    | _            -> raise Not_found
    (* TODO edit coordinates *)
