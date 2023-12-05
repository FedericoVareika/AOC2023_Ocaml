open Base
open Stdio 

let charlist_of_str str = 
    let rec aux i l = 
        if i < String.length str then 
            str.[i] :: aux (i+1) l 
        else l 
    in 
    aux 0 [] 
;;

let get_number (str : string) = 
    let rec aux fst snd chars = 
        match chars with 
        | [] -> fst * 10 + snd
        | x :: xs -> (
            match Char.escaped x |> Int.of_string_opt with 
            | None -> aux fst snd xs
            | Some y -> if Int.equal fst (-1) then 
                aux y y xs
            else aux fst y xs)
    in 
    charlist_of_str str |> aux (-1) (-1) 
;;

let () = 
    let lines = Stdio.In_channel.read_lines "./inputs/day01.txt" in 
    let list = List.map ~f:get_number lines in 
    let result = List.fold list ~init:0 ~f:(+) in 
    printf "%i\n" result; 
;;

(*Part 2*)

let cases =
    [ "one", 1
    ; "two", 2
    ; "three", 3
    ; "four", 4
    ; "five", 5
    ; "six", 6
    ; "seven", 7
    ; "eight", 8
    ; "nine", 9
    ; "1", 1
    ; "2", 2
    ; "3", 3
    ; "4", 4
    ; "5", 5
    ; "6", 6
    ; "7", 7
    ; "8", 8
    ; "9", 9
    ] 
;;

let get_number_2 str = 
    let rec aux i dif = 
        let tuple_of_is_substring_at (k, v) = 
            (v, String.is_substring_at str ~pos:i ~substring:k)
        in
        let list = List.map cases ~f:tuple_of_is_substring_at in
        let values_list = List.filter_map list ~f:(fun (v, b) -> 
            if b then Some v else None) in 
        match List.nth values_list 0 with 
        | None -> aux (i+dif) dif 
        | Some v -> v 
    in 
    (aux 0 1) * 10 + (aux (String.length str - 1) (-1))
;;        

let () = 
    let lines = Stdio.In_channel.read_lines "./inputs/day01.txt" in 
    let list = List.map ~f:get_number_2 lines in 
    let result = List.fold list ~init:0 ~f:(+) in 
    printf "%i\n" result; 
;;
