open Base 
open Stdio 
 
type elem = 
    { value : string 
    ; l : string
    ; r : string 
    }
;;

type map = 
    { instructions : string
    ; elements : elem list 
    }
;;

let elem_of_string str = 
    let str = String.substr_replace_all str ~pattern:" " ~with_:"" in 
    let str = String.substr_replace_all str ~pattern:"(" ~with_:"" in 
    let str = String.substr_replace_all str ~pattern:")" ~with_:"" in 
    match String.split ~on:'=' str with 
    | value :: lr :: [] -> (
        match String.split ~on:',' lr with 
        | l :: r :: [] -> 
            (* printf "value: %s, l: %s, r: %s \n" value l r;  *)
            {value ; l ; r} 
        | _ -> failwith "second split wrong" )
    | _ -> failwith "first split wrong"
;;

let make_map lines = 
    match lines with 
    | instructions :: _ :: elements -> 
        let elements = List.map elements ~f:elem_of_string in 
        {instructions ; elements}
    | _ -> failwith "error parsing"
;;

let get_path_length map = 
    let rec aux index next = 
        if String.equal next "ZZZ" then index 
        else 
            let at = index % (String.length map.instructions) in 
            match String.get map.instructions at with 
            | 'L' ->
                let next = List.find_exn map.elements ~f:(fun elem -> 
                    String.equal elem.value next) in 
                aux (index + 1) next.l 
            | 'R' -> 
                let next = List.find_exn map.elements ~f:(fun elem -> 
                    String.equal elem.value next) in 
                aux (index + 1) next.r
            | _ -> failwith "error instruction parsing"
    in 
    aux 0 "AAA"
;; 

let () = 
    let lines = In_channel.read_lines "./inputs/day08.txt" in 
    let map = make_map lines in 
    let result = get_path_length map in 
    printf "Part 1: %i\n" result 

(* Part 2 *)

let get_hcf a b = 
    let rec aux idx hcf = 
        if idx > a || idx > b then 
            hcf 
        else if (Int.equal (a % idx) 0) && (Int.equal (b % idx) 0) then 
            aux (idx + 1) idx
        else 
            aux (idx + 1) hcf 
    in 
    aux 2 1

let get_path_length map start = 
    let rec aux index next = 
        match String.to_list next with 
        | [_ ; _ ; 'Z'] -> index 
        | _ -> 
            let at = index % (String.length map.instructions) in 
            match String.get map.instructions at with 
            | 'L' ->
                let next = List.find_exn map.elements ~f:(fun elem -> 
                    String.equal elem.value next) in 
                aux (index + 1) next.l 
            | 'R' -> 
                let next = List.find_exn map.elements ~f:(fun elem -> 
                    String.equal elem.value next) in 
                aux (index + 1) next.r
            | _ -> failwith "error instruction parsing"
    in 
    aux 0 start 
;; 

let min_path_length map = 
    let starts = List.filter map.elements ~f:(fun elem -> 
        match String.to_list elem.value with 
        | [_ ; _ ; 'A'] -> true 
        | _ -> false ) in 
    List.map starts ~f:(fun elem -> get_path_length map elem.value) 
        |> List.fold ~init:1 ~f:(fun acc path -> 
            printf "acc: %i, path: %i " acc path;
            printf "hcf: %i\n" @@ get_hcf acc path; 
            (acc * path) / (get_hcf acc path)) 

let () = 
    let lines = In_channel.read_lines "./inputs/day08.txt" in 
    let map = make_map lines in 
    let result = min_path_length map in 
    printf "Part 2: %i\n" result 

