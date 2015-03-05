open Map_data_t
open Core.Std

type t = Map_data_t.port

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
