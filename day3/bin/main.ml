let file_channel = open_in "./input.txt"

(* Helper functions *)
let _flatten xss = List.map (fun xs -> List.nth xs 0) xss ;;
let _max xs = List.fold_left max min_int xs ;;
let _sum xs = List.fold_left ( + ) 0 xs ;;

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

let _split_by delimiter str = 
  Str.split (Str.regexp delimiter) str 
;;

let list_product list =
  List.fold_left ( * ) 1 list
;;

(* Solution *)

let lines_raw = read_lines file_channel []
let lines = List.filter (fun line -> String.length line > 0) lines_raw


type num_location = {num: string; line_number: int; start: int}

let rec _find_numbers start numbers line line_number = 
  try 
    let _ = Str.search_forward (Str.regexp "[0-9]+") line start in
    let num = Str.matched_string line in
    let match_start = Str.match_beginning () in
    let index_after_match = Str.match_end () in
    {num = num; line_number = line_number; start = match_start} :: (_find_numbers index_after_match numbers line line_number)
  with _ -> numbers
;;

let find_numbers = _find_numbers 0 []

type symbol_location = {symbol: string; line_num: int; pos: int}

let rec _find_symbols start symbols line line_num = 
  try 
    let _ = Str.search_forward (Str.regexp "[^0-9.]") line start in
    let symbol = Str.matched_string line in
    let pos = Str.match_beginning () in
    let index_after_match = pos + 1 in
    {symbol = symbol; line_num = line_num; pos = pos} :: (_find_symbols index_after_match symbols line line_num)
  with _ -> symbols
;;

let find_symbols = _find_symbols 0 []

let nums_per_line = List.mapi (fun idx line -> find_numbers line idx) lines
let all_nums = List.flatten nums_per_line

let symbols_per_line = List.mapi (fun idx line -> find_symbols line idx) lines
let all_symbols = List.flatten symbols_per_line

let is_in_neighborhood line_num1 line_num2 =
  Int.abs (line_num1 - line_num2) <= 1
;;

let find_numbers_around_symbol number_locations symbol_location =
    List.filter 
      (fun num_loc -> is_in_neighborhood num_loc.line_number symbol_location.line_num) 
      number_locations
;;

let is_number_in_area num_location symbol_location = 
  let num_start = num_location.start in
  let num_end = num_location.start + String.length num_location.num in
  let sym_pos = symbol_location.pos in
  num_start - 1 <= sym_pos && sym_pos <= num_end
;;

let num_locs_in_area_of_symbol number_locations symbol_location = 
  let nums_around = find_numbers_around_symbol number_locations symbol_location in
  List.filter (fun num_loc -> is_number_in_area num_loc symbol_location) nums_around
;;


let part_num_locss = List.map (fun sym_loc -> num_locs_in_area_of_symbol all_nums sym_loc) all_symbols
let part_num_locs = List.flatten part_num_locss
let part_nums = List.map (fun num_loc -> int_of_string num_loc.num) part_num_locs
let part_nums_sum = _sum part_nums
let () = Printf.printf "Part1: %d\n" (part_nums_sum)


(* Part 2 *)

let gear_locs = List.filter (fun sym_loc -> sym_loc.symbol = "*") all_symbols

let gear_part_num_locss = List.map (fun sym_loc -> num_locs_in_area_of_symbol all_nums sym_loc) gear_locs
let gear_part_num_locs_2_numberss = List.filter (fun num_locs -> List.length num_locs = 2) gear_part_num_locss
let gear_part_nums_2_numberss = List.map (List.map (fun num_loc -> int_of_string num_loc.num)) gear_part_num_locs_2_numberss
let gear_ratios = List.map list_product gear_part_nums_2_numberss
let sum_gear_ratios = _sum gear_ratios

let () = Printf.printf "Part2: %d\n" sum_gear_ratios
