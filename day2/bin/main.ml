let file_channel = open_in "./input.txt"  


let rec _length acc l =
  match l with
  | [] -> acc
  | _ :: tail -> _length (acc+1) tail;;


let rec sum xs initial = 
  match xs with
  | [] -> initial
  | head :: tail -> head + sum tail initial
;;


let rec read_lines in_channel lines = 
  try
  let line = input_line in_channel in
    read_lines in_channel (lines @ [line])
  with _ -> 
    lines
;;


let lines_raw = read_lines file_channel []
let lines = List.filter (fun line -> String.length line > 0) lines_raw


exception Invalid_arg


let _print_list xs =
  match xs with
  (* | [_] :: _ -> List.iter print_list xs *)
  | [] -> print_endline "empty"
  | x :: [] -> print_endline x
  | hd :: tl -> 
    List.iter (Printf.printf "%s, ") (hd :: tl);
    print_endline ""
;;



let _convert_count_to_number count_and_color =
  match count_and_color with
  | count :: color -> 
    (* List.iter (Printf.printf "%s\n") color; *)
    count :: color
  | _ -> 
    (* List.iter print_endline s; *)
    raise Invalid_arg
;;
  

let to_tuple x__y =
  (* Printf.printf "(%s, %s)\n" (List.nth x__y 0) (List.nth x__y 1); *)
  ((List.nth x__y 0), (List.nth x__y 1))
;;

let _to_tuples count_and_colors =
  List.map to_tuple count_and_colors
;;


let split_by delimiter str = 
  Str.split (Str.regexp delimiter) str 
;;

type game_record = {game_id: string; rounds: string list list list }

(* -> [[3; blue]; [1; red]] *)
let parse_color_counts line = 
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  let parts = Str.split (Str.regexp ": ") line in 
  (* ["Game 1"; "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"] *)
  let game_col = List.nth parts 0 in
  (* "Game 1" *)
  let game_id = List.nth (split_by " " game_col) 1 in
  (* "1" *)
  let cubess = List.nth parts 1 in
  (* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  let draws = Str.split (Str.regexp "; ") cubess in
  (* ["3 blue, 4 red"; "1 red, 2 green, 6 blue"; ["2 green"] *)
  let count_and_colors = List.map (Str.split (Str.regexp ", ")) draws in
  (* [["3 blue"; "4 red"]; ["1 red"; "2 green"; "6 blue"]; ["2 green"] *)
  let ts = List.map (List.map (split_by " ")) count_and_colors in
  (* [[["3"; "blue"]; ["4"; "red"]]; [["1"; "red"]; ["2"; "green"]; ["6"; "blue"]]; [["2"; "green"]]] *)
  {game_id = game_id; rounds = ts};
;;

let () = print_endline ""

let parsed_games = List.map parse_color_counts lines

let _n_red = 12
let _n_green = 13
let _n_blue = 14


(* draw: ["n"; "color"] *)
let is_draw_possible draw =
  match draw with
  | n :: ["red"] -> int_of_string(n) <= _n_red
  | n :: ["green"] -> int_of_string(n) <= _n_green
  | n :: ["blue"] -> int_of_string(n) <= _n_blue
  | _ -> raise Invalid_arg
;;

(* round: [["n"; "color"]; ["n"; "color"]] *)
let are_draws_possible round =
  List.for_all is_draw_possible round
;;

(* game: { game_id = "id"; rounds = [[["n"; "color"]]; [["n"; "color"]; ["n"; "color"]]] } *)
let is_game_possible game = 
  List.for_all are_draws_possible game.rounds
;;

let possible_games = List.filter is_game_possible parsed_games


(* let print_game_id game = print_endline game.game_id ;;

let () = List.iter print_game_id possible_games *)

let _possible_game_ids_as_strs = List.map (fun game -> game.game_id) possible_games
let possible_game_ids = List.map int_of_string _possible_game_ids_as_strs
let solution = sum possible_game_ids 0

let () = Printf.printf "Part1: %d\n" solution


let () = close_in file_channel