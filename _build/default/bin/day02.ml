open Base 
open Stdio 

let counter = 
    [ "red", 12
    ; "green", 13
    ; "blue", 14]
;;

let get_blocks str = 
    let blocks = String.split_on_chars ~on:[','] str in 
    List.map blocks ~f:(fun elem -> 
        let trimmed = String.strip elem in
        let list = trimmed |> String.split_on_chars ~on:[' '] in 
        (List.last_exn list, List.hd_exn list |> Int.of_string)
    )
;;

let set_is_valid str = 
    let blocks = get_blocks str in 
    List.map blocks ~f:(fun (color, count) -> 
        let (_, v) = List.find_exn counter ~f:(fun (k, _) -> 
            String.equal color k) in 
        count <= v
    ) |> List.fold ~init:true ~f:(fun acc b -> acc && b)
;;

let () = 
    let lines = In_channel.read_lines "./inputs/day02.txt" in 
    let games = List.map lines ~f:(fun line -> 
        let list = String.split_on_chars ~on:[':'] line in
        let sets = (List.last_exn list 
            |> String.split_on_chars ~on:[';']
            |> List.map ~f:(fun str -> set_is_valid str)) in
        (List.hd_exn list, List.fold sets ~init:true ~f:(fun acc elem -> acc && elem)) 
    ) in 
    let games = List.filter games ~f:(fun (_, b) -> b) in
    let games = List.map games ~f:(fun (game, _) -> 
        String.split_on_chars ~on:[' '] game 
        |> List.last_exn 
        |> Int.of_string 
    ) in 
    let result = List.fold games ~init:0 ~f:(fun acc elem -> acc + elem) in  
    printf "Part 1: %i\n" result
;;

(* Part 2 *)

let power_of_game game = 
    let list = String.split_on_chars ~on:[':'] game in 
    let sets = String.split_on_chars ~on:[';'] (List.last_exn list) in 
    let sets = List.map sets ~f:(fun set -> get_blocks set) in 
    let get_block_of_color color list = (
        let block = List.find ~f:(fun (k, _) -> String.equal k color) list in
        match block with 
        | Some (_, v) -> Some v
        | None -> None
    ) in 
    let (reds, greens, blues) = (
          List.map sets ~f:(fun blocks -> get_block_of_color "red" blocks)
        , List.map sets ~f:(fun blocks -> get_block_of_color "green" blocks)
        , List.map sets ~f:(fun blocks -> get_block_of_color "blue" blocks)
    ) in 
    let (red, green, blue) = (
          List.filter_opt reds |> List.max_elt ~compare:Int.compare  
        , List.filter_opt greens |> List.max_elt ~compare:Int.compare 
        , List.filter_opt blues |> List.max_elt ~compare:Int.compare 
    ) in 
    match (red, green, blue) with 
    | (Some r, Some g, Some b) -> r * g * b 
    | _ -> 0

;;

let () = 
    let lines = In_channel.read_lines "./inputs/day02.txt" in
    let res = (List.map lines ~f:power_of_game 
        |> List.fold ~init:0 ~f:(fun acc value -> acc + value) 
    ) in 
    printf "Part 2: %i\n" res

    
