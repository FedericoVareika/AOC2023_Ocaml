open Base
open Stdio 

type direction = 
    Start | W | E | N | S | X 
;;

let dir_to_string dir = 
    match dir with 
    | Start -> "Start"
    | X -> "X"
    | N -> "N" 
    | E -> "E"
    | S -> "S"
    | W -> "W"
;;

let mapping = 
    [ ('S', Start, Start)
    ; ('-', W, E)
    ; ('|', N, S)
    ; ('L', N, E)
    ; ('J', N, W)
    ; ('7', S, W)
    ; ('F', S, E)
    ; ('.', X, X)
    ]
;;

let get_at map (x, y) = 
    match List.nth map y with 
    | None -> None 
    | Some row -> List.nth row x 
;;

let move ~pos:(x, y) dir = 
    match dir with 
    | W -> x - 1, y
    | E -> x + 1, y
    | N -> x, y - 1
    | S -> x, y + 1 
    | _ -> x, y
;;

let opposite dir = 
    match dir with 
    | W -> E 
    | E -> W 
    | N -> S 
    | S -> N 
    | _ -> dir
;;

let loop_length ~map:map ~start:(x, y) = 
    let rec length ~acc:acc ~pos:pos ~from:from = 
        let elem = get_at map pos in
        match elem with 
        | None -> 0 
        | Some (X, X) -> 0  
        | Some (_, Start) | Some (Start, _) -> acc 
        | Some (f, n) when phys_equal f from ->  
            let pos' = move ~pos:pos n in 
            let from' = opposite n in 
            length ~acc:(acc + 1) ~pos:pos' ~from:from' 
        | Some (n, f) when phys_equal f from ->  
            let pos' = move ~pos:pos n in 
            let from' = opposite n in 
            length ~acc:(acc + 1) ~pos:pos' ~from:from' 
        | _ -> 0 
    in 
    let l1 = length ~acc:0 ~pos:(x+1, y) ~from:W in 
    let l2 = length ~acc:0 ~pos:(x, y+1) ~from:N in 
    let l3 = length ~acc:0 ~pos:(x-1, y) ~from:E in 
    let l4 = length ~acc:0 ~pos:(x, y-1) ~from:S in 
    max l1 l2 |> max l3 |> max l4
;;

let map_char char = 
    let _, d1, d2 = List.find_exn mapping ~f:(fun (c, _, _) -> Char.equal c char) in
    d1, d2
;;

let find_start map = 
    let rec aux x y map = 
        match map with 
        | [] -> x, y 
        | [] :: xs -> aux 0 (y + 1) xs 
        | (i :: ys) :: xs -> 
            match i with 
            | Start, Start -> x, y
            | _ -> aux (x + 1) y (ys :: xs)
    in 
    aux 0 0 map 
;;

let print_map map = 
    List.iter map ~f:(fun row -> 
        List.iter row ~f:(fun (d1, d2) -> 
            printf "(%s, %s)" (dir_to_string d1) (dir_to_string d2)); 
        printf "\n")
;;

let () = 
    printf "\n";
    let lines = In_channel.read_lines "./inputs/day10.txt" in 
    let map = List.map lines ~f:(fun line -> 
        String.to_list line 
            |> List.map ~f:map_char) in 
    (* print_map map; *)
    let (x, y) = find_start map in 
    (* printf "Start pos: (%i, %i)\n" x y; *)
    let length = loop_length ~map:map ~start:(x, y) in 
    let diameter = Float.(/) (Int.to_float length) 2.0 in
    let diameter = Float.round_up diameter in 
    printf "Part 1: %f\n" diameter;
    ()
;; 

