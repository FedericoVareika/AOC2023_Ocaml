open Base 
open Stdio 

let dirs = 
    [ (-1, -1) ; (-1, 0) ; (-1, 1) 
    ; (0, -1) ; (0, 1) 
    ; (1, -1) ; (1, 0) ; (1, 1)]
;;

let is_symbol str pos dx = 
    let aux char = 
        match char with 
        | '0'..'9' | '.' -> false
        | _ -> true 
    in 
    match str.[pos + dx] with 
    | char -> aux char
    | exception Invalid_argument _ -> false
;;

let is_symbol_opt opt_str pos dx = 
    match opt_str with 
    | Some str -> is_symbol str pos dx 
    | None -> false 
;;

let get_line_sum above line below = 
    let is_valid_number pos = 
        List.fold_left dirs ~init:false ~f:(fun acc (y, x) -> 
            let is_gear = match y with 
                | -1 -> is_symbol_opt above pos x 
                | 0 -> is_symbol line pos x 
                | 1 -> is_symbol_opt below pos x 
                | _ -> false
            in 
            acc || is_gear ) 
    in
    let rec aux pos sum current_number is_valid = 
        let updated_sum = if is_valid then 
                sum + (Int.of_string current_number)
            else sum in
        if Int.equal pos (String.length line) then 
            updated_sum 
        else 
            match line.[pos] with 
            | '0'..'9' ->
                let is_valid = is_valid_number pos || is_valid in
                aux (pos+1) sum (current_number ^ String.make 1 line.[pos]) is_valid
            | _ -> 
                let pos = pos + 1 in 
                aux pos updated_sum "" false 
    in 
    aux 0 0 "" false 
;;

let get_sum chan init = 
    let rec aux above opt_line sum = 
        let opt_below = In_channel.input_line chan in 
        match opt_line with 
        | Some line -> 
            let sum = sum + get_line_sum above line opt_below in 
            aux opt_line opt_below sum 
        | None -> match opt_below with 
            | Some _ -> aux None opt_below sum  
            | None -> sum 
    in 
    aux None None init 
;;

let () = 
    let chan = In_channel.create "./inputs/day03.txt" in 
    let result = get_sum chan 0 in 
    printf "Part 1: %i\n" result 
;; 

(*Part 2*)

type gear = 
    { row : int 
    ; col : int 
    } 
    (* [@@deriving show] *)

let get_gears_from_line line row = 
    let characters = String.to_list line in 
    List.filter_mapi characters ~f:(fun col char -> 
        match char with 
        | '*' -> Some { row ; col } 
        | _ -> None ) 
;;

type num = 
    { value : int 
    ; row : int 
    ; col_start : int 
    ; col_end : int 
    } 
    (* [@@deriving show] *)

let get_nums_from_line line row = 
    let characters = String.to_list line in 
    List.foldi characters ~init:[] ~f:(fun col acc char -> 
        match char with 
        | '0'..'9' -> (
            let int_char = Int.of_string (String.make 1 char) in 
            match acc with 
            | [] ->  { value = int_char ; row ; col_start = col ; col_end = col } :: [] 
            | x :: xs -> if Int.equal x.col_end (col - 1) then 
                { value = x.value * 10 + int_char
                ; row 
                ; col_start = x.col_start 
                ; col_end = col 
                } :: xs 
            else 
                { value = int_char 
                ; row 
                ; col_start = col 
                ; col_end = col } :: acc ) 
        | _ -> acc 
    )
;;

let nums_of_gear (gear: gear) numslist = 
    let rec aux nums acc = 
        match nums with 
        | [] -> acc 
        | { value ; row ; col_start ; col_end } :: xs -> 
            if row <= gear.row + 1 && row >= gear.row - 1 
                && (col_start <= gear.col + 1 && col_start >= gear.col - 1 || 
                col_end <= gear.col + 1 && col_end >= gear.col - 1) then
                    aux xs (value :: acc)
            else 
                aux xs acc
     in 
    aux numslist [] 
;;
        


let () = 
    let lines = In_channel.read_lines "./inputs/day03.txt" in 
    let gearslist = List.mapi lines ~f:(fun idx line -> get_gears_from_line line idx) in 
    let gearslist = List.concat gearslist in 
    let numslist = List.mapi lines ~f:(fun idx line -> get_nums_from_line line idx) in 
    let numslist = List.concat numslist in 
    (* List.iter gearslist ~f:(Fmt.pr "%a@." pp_gear); *)
    (* List.iter numslist ~f:(Fmt.pr "%a@." pp_num); *)
    let nums_of_gears = List.map gearslist ~f:(fun gear -> 
        nums_of_gear gear numslist 
    ) in 
    (* List.iter nums_of_gears ~f:(fun list ->  *)
    (*     List.iter list ~f:(printf "%i, "); *)
    (*     print_endline "" *)
    (* ); *)
    let multiplied_gears = List.map nums_of_gears ~f:(fun nums -> 
        if List.length nums > 1 then 
            List.reduce_exn nums ~f:( * )
        else 0
    ) in 
    let result = List.reduce_exn multiplied_gears ~f:(+) in 
    printf "Part 2: %i\n" result
;;
