let input_test = "./inputs/day2/test.txt"
let input_file = "./inputs/day2/input.txt"

(** how many cubes of each color were shown by the elf *)
type cubes =
  | Red of int
  | Green of int
  | Blue of int

(** one of the games played with the elf *)
type game =
  { id : int
  ; reds : int
  ; greens : int
  ; blues : int
  }

(** this is a "fake game" representing the maximum possible number of cubes of
    each color *)
let max_game = { id = -1; reds = 12; greens = 13; blues = 14 }

(** true if none of the cubes of the game exceed the maximum number of cubes
    (part 1) *)
let is_valid_game g =
  g.reds <= max_game.reds && g.blues <= max_game.blues && g.greens <= max_game.greens
;;

(** the power of a game is the product of the number of cubes of each color
    (part 2) *)
let power_of_game g = g.reds * g.greens * g.blues

(** from a game line (a string) extract the game id. Returns a tuple containing
    the id and the rest of the line.
    An exception is raised if the input is invalid *)
let parse_game_id s =
  (* length of "Game " *)
  let skip = 5 in
  let sub_len = String.length s - skip in
  (* skip the "Game " part of the string *)
  let s = String.sub s skip sub_len in
  match String.split_on_char ':' s with
  | [ id; rest ] -> int_of_string id, rest
  | _ -> failwith "invalid input"
;;

(** each set of cubes is a comma-separated list of cubes. Each cube is a
    space-separated pair of a number and a color. This function parses a cube
    and returns the corresponding type (Red, Green or Blue) with the number of
    cubes of this color.
    An exception is raised if the input is invalid *)
let parse_cubes c =
  match c |> String.trim |> String.split_on_char ' ' with
  | [ n; "blue" ] -> Blue (int_of_string n)
  | [ n; "red" ] -> Red (int_of_string n)
  | [ n; "green" ] -> Green (int_of_string n)
  | _ -> failwith "invalid input"
;;

(** parse a set of cubes. A set is a semicolon-separated list of cubes *)
let parse_set set = set |> String.split_on_char ',' |> List.map parse_cubes

(** parse the sets section of a game line. A set is a semicolon-separated list
    of cubes *)
let parse_game_sets line =
  line |> String.split_on_char ';' |> List.map parse_set |> List.flatten
;;

(** accumulate the number of cubes of each color in a game *)
let acc_game_cubes game = function
  | Blue n -> { game with blues = (if n > game.blues then n else game.blues) }
  | Red n -> { game with reds = (if n > game.reds then n else game.reds) }
  | Green n -> { game with greens = (if n > game.greens then n else game.greens) }
;;

(** parse a game line. A game line is a string of the form "Game #id: set1;
    set2; set3; ..." *)
let parse_game_line line =
  let id, rest = parse_game_id line in
  let game = { id; reds = 0; greens = 0; blues = 0 } in
  let cubes = parse_game_sets rest in
  List.fold_left acc_game_cubes game cubes
;;

(** common function for part 1 and part 2 *)
let common file = Utils.read_input file |> List.map parse_game_line

(* solution of the part 1 *)
let part1 =
  common input_file
  |> List.filter is_valid_game
  |> List.fold_left (fun acc game -> acc + game.id) 0
;;

(* solution of the part 2 *)
let part2 =
  common input_file |> List.fold_left (fun acc game -> acc + power_of_game game) 0
;;
