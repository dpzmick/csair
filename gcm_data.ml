open Core.Std

type t = string list

let from g =
    let all = (Graph.all_routes g) in
    List.fold all
        ~init:[]
        ~f:(fun acc route ->
            (sprintf "%s-%s" (Port.code (Route.from_port route)) (Port.code (Route.to_port route)))::acc
        )

let string_of_t strings =
    let args = List.intersperse strings ~sep:"%2C+" in
    let folded = List.fold args ~init: "" ~f:(fun acc el -> sprintf "%s%s" acc el) in
    let header = "http://www.gcmap.com/mapui?P=" in
    let footer = "&MS=wls&DU=mi" in
    sprintf "%s%s%s" header folded footer
