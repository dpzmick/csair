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

    let compare p1 p2 =
        let l = (String.equal p1.code p2.code)::[] in
        let l = (String.equal p2.name p2.name)::l in
        let l = (String.equal p2.country p2.country)::l in
        let l = (p1.timezone = p2.timezone)::l in
        let l = (Coordinates.equal p1.coordinates p2.coordinates)::l in
        let l = (p1.population = p2.population)::l in
        let l = (p1.region = p2.region)::l in
        if List.fold l ~init:true ~f:(&&)
        then String.compare p1.code p2.code
        else -1

    (* TODO these are massive hacks!! *)
    let sexp_of_t p = String.sexp_of_t p.name
    let t_of_sexp _ = empty
end

include T
include Comparable.Make(T)

let code       p = p.code
let name       p = p.name
let timezone   p = p.timezone
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
