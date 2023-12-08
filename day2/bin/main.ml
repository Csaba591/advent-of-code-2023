let file_channel = open_in "./input.txt"  

(* Helper functions *)
let flatten xss = List.map (fun xs -> List.nth xs 0) xss ;;
let max xs = List.fold_left max min_int xs ;;
let sum xs = List.fold_left ( + ) 0 xs ;;

let rec _length acc l =
  match l with
  | [] -> acc
  | _ :: tail -> _length (acc+1) tail;;


let rec read_lines in_channel lines = 
  try
  let line = input_line in_channel in
    read_lines in_channel (lines @ [line])
  with _ -> 
    lines
;;

let split_by delimiter str = 
  Str.split (Str.regexp delimiter) str 
;;


(* Solution *)

let lines_raw = read_lines file_channel []
let lines = List.filter (fun line -> String.length line > 0) lines_raw

exception Invalid_arg

type game_record = {game_id: string; rounds: string list list list }

(* -> [["3"; "blue"]; ["1"; "red"]] *)
let parse_games line = 
  (* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
  let parts = Str.split (Str.regexp ": ") line in 
  (* ["Game 1"; "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"] *)
  let game_col = List.nth parts 0 in
  (* "Game 1" *)
  let game_id = List.nth (split_by " " game_col) 1 in
  (* "1" *)
  let dices = List.nth parts 1 in
  (* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" *)
  let draws = Str.split (Str.regexp "; ") dices in
  (* ["3 blue, 4 red"; "1 red, 2 green, 6 blue"; ["2 green"] *)
  let count_and_colors = List.map (Str.split (Str.regexp ", ")) draws in
  (* [["3 blue"; "4 red"]; ["1 red"; "2 green"; "6 blue"]; ["2 green"] *)
  let ts = List.map (List.map (split_by " ")) count_and_colors in
  (* [[["3"; "blue"]; ["4"; "red"]]; [["1"; "red"]; ["2"; "green"]; ["6"; "blue"]]; [["2"; "green"]]] *)
  {game_id = game_id; rounds = ts};
;;


let parsed_games = List.map parse_games lines

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

let possible_game_ids_as_strs = List.map (fun game -> game.game_id) possible_games
let possible_game_ids = List.map int_of_string possible_game_ids_as_strs
let solution = sum possible_game_ids

let () = Printf.printf "Part1: %d\n" solution

let draw_is_of_color color draw =
  let c = List.nth draw 1 in
  c = color
;;

(* -> [["n"; "color"]] *)
let _get_draw_of_color_from_round color round =
  List.filter (draw_is_of_color color) round
;;

(* [[["n"; "red"]]; [["n"; "red"]]; [["n"; "red"]]] *)
let get_draws_of_color color rounds = 
  let draws = List.map (_get_draw_of_color_from_round color) rounds in
  List.filter (fun xs -> List.length xs > 0) draws
;;

let game_to_max_of_each_color rounds color = 
  let draws = get_draws_of_color color rounds in
  let counts_str = flatten (flatten draws) in
  let counts = List.map int_of_string counts_str in
  max counts
;;

let game_to_max_of_each_color rounds = 
  let colors = ["red"; "green"; "blue"] in
  List.map (game_to_max_of_each_color rounds) colors
;;

let game_rounds_to_powers games =
  let max_of_each_color_per_game = List.map (fun game -> game_to_max_of_each_color game.rounds) games in  
  let powers = List.map (List.fold_left ( * ) 1) max_of_each_color_per_game in
  powers
;;

let powers = game_rounds_to_powers parsed_games
let sum_of_powers = sum powers

let () = Printf.printf "Part2: %d\n" sum_of_powers


let () = close_in file_channel