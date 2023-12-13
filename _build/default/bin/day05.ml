open Base
open Stdio 

let get_seeds chan = 
    match In_channel.input_line chan with 
    | Some seeds -> 
        List.tl_exn (String.split ~on:' ' seeds) |> 
        List.map ~f:(fun seed -> (Int.of_string seed, false)) 
    | None -> [] 
;;

let map list ~mapping:mapping =
    let list' = String.split mapping ~on:' ' in 
    let dest, src, range = match List.map list' ~f:(Int.of_string) with 
    | dest :: src :: range :: [] -> dest, src, range 
    | _ -> 0, 0, 0 in
    List.map list ~f:(fun (num, checked) -> 
        if num >= src && num < src + range && not checked then 
            (dest + (num - src), true)
        else (num, checked)
    )

let rec run chan list = 
    match In_channel.input_line chan with 
    | None -> list 
    | Some "" -> let _ = In_channel.input_line chan in  
        run chan (List.map list ~f:(fun (num, _) -> num, false))
    | Some mapping -> run chan (map list ~mapping:mapping)
;;

let () = 
    let chan = In_channel.create "./inputs/day05.txt" in 
    let seeds = get_seeds chan in 
    let list = run chan seeds in 
    let result = List.fold list ~init:(Int.max_value) ~f:(fun acc (num, _) -> 
        if num < acc then num else acc 
    ) in 
    printf "Part 1: %i\n" result
;;

(*Part 2*)

let oc = Out_channel.create "./log" ;;  

let rec get_pairs list = 
    match list with 
    | x :: y :: xs -> (x, y, false) :: get_pairs xs 
    | _ -> [] 
;; 

let get_seeds_2 chan = 
    match In_channel.input_line chan with 
    | Some seeds -> 
        List.tl_exn (String.split ~on:' ' seeds) |> 
        List.map ~f:(fun seed -> (Int.of_string seed)) |>
        get_pairs  
    | None -> [] 
;;

let map_single_num dest src range' (start, range, checked) = 
    let x1, x2, y1, y2 = start, start + range - 1, src, src + range' - 1 in 
    Out_channel.fprintf oc "%i, %i, %i, %i\n" x1 x2 y1 y2; 
    if x1 <= y2 && y1 <= x2 then 
        match x1, y1, x2, y2, checked with
        | _, _, _, _, true -> [(start, range, checked)]
        | x1, y1, x2, y2, _ when x1 < y1 && x2 <= y2 -> 
            [(x1, y1 - x1, false) ; (dest, x2 - y1 + 1, true)]
        | x1, y1, _, y2, _ when x1 < y1 -> 
            [(x1, y1 - x1, false) ; (dest, y2 - y1 + 1, true) ; (y2 + 1, x2 - y2, false)]
        | x1, _, x2, y2, _ when x2 <= y2 -> 
            [(x1 - y1 + dest, x2 - x1 + 1, true)]
        | x1, _, x2, y2, _ -> 
            [(x1 - y1 + dest, y2 - x1 + 1, true) ; (y2 + 1, x2 - y2, false)]
    else [(start, range, checked)]

let rec flatten list = 
    match list with 
    | [] -> [] 
    | x :: xs -> 
        match x with 
        | [] -> flatten xs 
        | y :: ys -> 
            y :: (flatten (ys :: xs))
;;

let map_2 nums ~mapping:mapping = 
    let list' = String.split mapping ~on:' ' in 
    let dest, src, range = match List.map list' ~f:(Int.of_string) with 
    | dest :: src :: range :: [] -> dest, src, range 
    | _ -> 0, 0, 0 in
    List.map nums ~f:(map_single_num dest src range) |> 
    flatten 
;;    
            
let rec run_2 chan nums = 
    print_endline "here";
    match In_channel.input_line chan with 
    | None -> nums 
    | Some "" -> let _ = In_channel.input_line chan in  
        run_2 chan (List.map nums ~f:(fun (num, range, _) -> num, range, false))
    | Some mapping -> run_2 chan (map_2 nums ~mapping:mapping)
;;

let () = 
    let chan = In_channel.create "./inputs/day05.txt" in 
    let seeds = get_seeds_2 chan in 
    let list = run_2 chan seeds in 
    let result = List.fold list ~init:(Int.max_value) ~f:(fun acc (num, _, _) -> 
        if num < acc then num else acc 
    ) in 
    printf "Part 2: %i\n" result
;;
            


