open Base 
open Stdio 

let points_for_card card = 
    let numbers = List.nth_exn (String.split ~on:':' card) 1 in 
    let (winning, yours) = 
        match (String.split ~on:'|' numbers) with 
        | x :: y :: [] -> (x, y)
        | _ -> ("", "")
    in 
    let (winning, yours) = 
        (String.split ~on:' ' winning, String.split ~on:' ' yours) in 
    let rec aux acc list = 
        match list with 
        | [] -> acc 
        | "" :: xs -> aux acc xs 
        | x :: xs -> 
            let matches = List.fold ~init:false ~f:(fun acc str -> acc || String.equal x str) winning in
            if matches then 
                match acc with 
                | 0 -> aux 1 xs 
                | _ -> aux (acc * 2) xs
            else aux acc xs 
    in 
    let result = aux 0 yours in 
    result
;;

let () = 
    print_endline "";
    let lines = In_channel.read_lines "./inputs/day04.txt" in 
    let points = List.map ~f:points_for_card lines in 
    let result = List.fold ~init:0 ~f:(fun acc pts -> acc + pts) points in 
    printf "Part 1: %i \n" result
;;

(*Part 2*)

let count_matches winning yours = 
    let rec aux acc list = 
        match list with 
        | [] -> acc 
        | "" :: xs -> aux acc xs 
        | x :: xs -> 
            let matches = List.find  ~f:(fun str -> String.equal x str) winning in
            if Option.is_some matches then 
                aux (acc + 1) xs 
            else aux acc xs 
    in 
    aux 0 yours 
;;

let update_cards_with_scratch cards scratch num = 
    let numbers = List.nth_exn (String.split ~on:':' scratch) 1 in 
    let (winning, yours) = 
        match (String.split ~on:'|' numbers) with 
        | x :: y :: [] -> (x, y)
        | _ -> ("", "")
    in 
    let (winning, yours) = 
        (String.split ~on:' ' winning, String.split ~on:' ' yours) in 
    let num_matches = count_matches winning yours in 
    let cards = Array.mapi cards ~f:(fun idx value -> 
        if idx > num && idx <= (num_matches + num) then 
            value + cards.(num) 
        else value 
    ) in 
    cards
;;

let () = 
    let lines = In_channel.read_lines "./inputs/day04.txt" in 
    let cards = Array.init (List.length lines) ~f:(fun _ -> 1) in 
    let cards = List.foldi lines ~init:cards ~f:(fun idx acc line -> 
        update_cards_with_scratch acc line idx) in 
    let result = Array.fold cards ~init:0 ~f:(fun acc value -> acc + value) in
    printf "Part 2: %i\n" result 
;;


