open Base 
open Stdio 

let array = ref (Array.init 13 ~f:(fun _ -> 0)) ;; 

let card_to_num = 
    [ '2', 0
    ; '3', 1
    ; '4', 2 
    ; '5', 3 
    ; '6', 4
    ; '7', 5 
    ; '8', 6
    ; '9', 7
    ; 'T', 8
    ; 'J', 9
    ; 'Q', 10
    ; 'K', 11 
    ; 'A', 12 
    ]

let get_num ~mapping:mapping card = 
    let (_, num) = List.find_exn mapping ~f:(fun (char, _) -> 
        Char.equal char card) in 
    num
;;

let array_from_hand ~mapping:mapping hand = 
    array := Array.map !array ~f:(fun _ -> 0); 
    let rec aux hand' = 
        match hand' with 
        | x :: xs -> 
            let num = get_num ~mapping:mapping x in 
            !array.(num) <- !array.(num) + 1; 
            aux xs 
        | [] -> ()
    in 
    aux hand 
;;

let get_type card =
    array_from_hand ~mapping:card_to_num (String.to_list card);
    let card_type = Array.fold !array ~init:0 ~f:(fun acc num -> 
        match num with 
        | 2 -> 1 + acc 
        | 3 -> 3 + acc 
        | 4 -> 5  
        | 5 -> 6
        | _ -> acc 
    ) in 
    card_type 
;;

let rec compare_by_card_nums ~mapping:mapping hand1 hand2 = 
    match hand1, hand2 with
    | [], _ | _, [] -> 0 
    | x :: xs, y :: ys -> 
        let num1, num2 = get_num ~mapping:mapping x, get_num ~mapping:mapping y in 
        if phys_equal (num1 - num2) 0 then 
            compare_by_card_nums ~mapping:mapping xs ys 
        else num1 - num2 
;;
    
let () = 
    let lines = In_channel.read_lines "./inputs/day07.txt" in 
    let lines = List.map lines ~f:(fun line -> 
        match String.split ~on:' ' line with 
        | x :: y :: [] -> (x, y)
        | _ -> ("", "")) in 
    let hands = List.sort lines ~compare:(fun (hand1, _) (hand2, _) -> 
        let type1 = get_type hand1 in 
        let type2 = get_type hand2 in 
        if phys_equal (type1 - type2) 0 then 
            compare_by_card_nums ~mapping:card_to_num (String.to_list hand1) (String.to_list hand2)
        else type1 - type2) in 
    let result = List.foldi hands ~init:0 ~f:(fun idx acc (_, bid) -> 
        (Int.of_string bid) * (idx + 1) + acc) in 
    printf "Part 1: %i\n" result
;;

(*Part 2*)

let () =
    array := Array.map !array ~f:(fun _ -> 0)
;;

let card_to_num_part_2 = 
    [ 'J', 0
    ; '2', 1
    ; '3', 2
    ; '4', 3 
    ; '5', 4 
    ; '6', 5
    ; '7', 6 
    ; '8', 7
    ; '9', 8
    ; 'T', 9
    ; 'Q', 10
    ; 'K', 11 
    ; 'A', 12 
    ]

let get_type_2 card =
    array_from_hand ~mapping:card_to_num_part_2 (String.to_list card);
    let jokers = !array.(0) in 
    if not (phys_equal jokers 0) then (
        !array.(0) <- 0;
        Array.sort !array ~compare:(fun a b -> b - a); 
        !array.(0) <- !array.(0) + jokers; 
    ) else (); 
    let card_type = Array.fold !array ~init:0 ~f:(fun acc num -> 
        match num with 
        | 2 -> 1 + acc 
        | 3 -> 3 + acc 
        | 4 -> 5  
        | 5 -> 6
        | _ -> acc 
    ) in 
    card_type 
;;

let () = 
    let lines = In_channel.read_lines "./inputs/day07.txt" in 
    let lines = List.map lines ~f:(fun line -> 
        match String.split ~on:' ' line with 
        | x :: y :: [] -> (x, y)
        | _ -> ("", "")) in 
    let hands = List.sort lines ~compare:(fun (hand1, _) (hand2, _) -> 
        let type1 = get_type_2 hand1 in 
        let type2 = get_type_2 hand2 in 
        if phys_equal (type1 - type2) 0 then 
            compare_by_card_nums ~mapping:card_to_num_part_2 (String.to_list hand1) (String.to_list hand2)
        else type1 - type2) in 
    (* List.iter hands ~f:(fun (card, bid) -> printf "(%s, %s)\n" card bid); *)
    let result = List.foldi hands ~init:0 ~f:(fun idx acc (_, bid) -> 
        (Int.of_string bid) * (idx + 1) + acc) in 
    printf "Part 2: %i\n" result
;;
