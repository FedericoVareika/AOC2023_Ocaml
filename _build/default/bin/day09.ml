open Base 
open Stdio 

let rec diff sequence = 
    match sequence with 
    | [] | [_] -> [] 
    | x :: y :: xs -> (x - y) :: (diff @@ y :: xs)
;;

let rec calculate_last_elem diffs = 
    match diffs with 
    | [x :: xs] -> x 
    | (x :: xs) :: (y :: ys) :: xxs -> 
        let diffs = ((x + y) :: ys) :: xxs in 
        calculate_last_elem diffs 
    | _ -> 0 
;;

let get_next_in_sequence sequence = 
    let rec aux diffs = 
        let last_diff = diff (List.hd_exn diffs) in 
        match List.find last_diff ~f:(fun d -> not (phys_equal d 0)) with 
        | Some _ -> aux (last_diff :: diffs)
        | None -> calculate_last_elem diffs 
    in 
    aux [List.rev sequence]
;;

let () = 
    let lines = In_channel.read_lines "./inputs/day09.txt" in 
    let result = List.map lines ~f:(fun str -> 
            String.split ~on:' ' str |> List.map ~f:Int.of_string)
        |> List.map ~f:get_next_in_sequence 
        |> List.fold ~init:0 ~f:(+) in 
    printf "Part 1: %i \n" result
;;

(*Part 2*)

let () = 
    printf "Hello \n";
    let lines = In_channel.read_lines "./inputs/day09.txt" in 
    let result = List.map lines ~f:(fun str -> 
            String.split ~on:' ' str |> List.map ~f:Int.of_string)
        |> List.map ~f:List.rev 
        |> List.map ~f:get_next_in_sequence  
        |> List.fold ~init:0 ~f:(+) in 
    printf "Part 2: %i" result
;;
