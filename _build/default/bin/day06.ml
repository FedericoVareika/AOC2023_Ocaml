open Base 
open Stdio 

let ways_to_win (time, distance) = 
    let rec aux counter = 
        match counter * (time - counter) with 
        | x when x > distance -> time + 1 - counter * 2 
        | _ -> aux (counter + 1)
    in 
    aux 0 
;; 

let get_time_distance line1 line2 = 
    let line1 = String.substr_replace_all ~pattern:"Time:      " ~with_:"" line1 in  
    let line2 = String.substr_replace_all ~pattern:"Distance:  " ~with_:"" line2 in  
    let times = String.split ~on:' ' line1 in 
    let distances = String.split ~on:' ' line2 in 
    let rec aux first second = 
        match first, second with 
        | [], _ | _, [] -> [] 
        | "" :: xs, _ -> aux xs second 
        | _, "" :: ys -> aux first ys 
        | x :: xs, y :: ys -> 
            (Int.of_string x, Int.of_string y) :: aux xs ys 
    in 
    aux times distances
;;

let () = 
    let line1, line2 = 
        match In_channel.read_lines "./inputs/day06.txt" with 
        | x :: y :: [] -> x, y
        | _ -> "", "" 
    in 
    let time_distance = get_time_distance line1 line2 in 
    let result = 
        List.map time_distance ~f:(ways_to_win) |> 
        List.fold ~init:1 ~f:( * ) in
    printf "Part 1: %i\n" result 
;; 

(*Part 2*)

let get_time_distance_2 line1 line2 = 
    let line1 = String.substr_replace_all ~pattern:"Time:      " ~with_:"" line1 in  
    let line2 = String.substr_replace_all ~pattern:"Distance:  " ~with_:"" line2 in  
    let time = String.substr_replace_all ~pattern:" " ~with_:"" line1 in 
    let distance = String.substr_replace_all ~pattern:" " ~with_:"" line2 in 
    (Int.of_string time, Int.of_string distance)
;;

let () = 
    let line1, line2 = 
        match In_channel.read_lines "./inputs/day06.txt" with 
        | x :: y :: [] -> x, y
        | _ -> "", "" 
    in 
    let time_distance = get_time_distance_2 line1 line2 in 
    let result = ways_to_win time_distance in 
    printf "Part 2: %i\n" result 
