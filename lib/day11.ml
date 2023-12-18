let input_test = "./inputs/day11/test.txt"
let input_file = "./inputs/day11/input.txt"

(* x, y *)
type point = int * int

(* positions of the galaxies in the map *)
module Galaxies = Set.Make (struct
    type t = point

    let compare = compare
  end)

let distance empties empty_val (gx, gy) (gx', gy') =
  let val_of p = if Galaxies.mem p empties then empty_val else 1 in
  let gen_list from to' = List.init (abs (from - to')) (fun i -> min from to' + i + 1) in
  let rows = gen_list gy gy' |> List.map (fun y -> val_of (0, y)) in
  let cols = gen_list gx gx' |> List.map (fun x -> val_of (x, 0)) in
  List.fold_left ( + ) 0 (rows @ cols)
;;

(* generate all pairs of elements in a list *)
let all_pairs l =
  let rec _aux acc = function
    | [] -> List.flatten acc |> List.rev
    | hd :: tl ->
      let pairs = List.fold_left (fun acc' el -> (hd, el) :: acc') [] tl in
      _aux (pairs :: acc) tl
  in
  _aux [] l
;;

(* calculate the sum of all minimum distances between galaxies *)
let add_all_distances empties empty_val galaxies =
  galaxies
  |> Galaxies.elements
  |> all_pairs
  |> List.fold_left (fun acc (p1, p2) -> acc + distance empties empty_val p1 p2) 0
;;

(* parsing section *)

type cell =
  | Empty
  | Galaxy

let cell_of_char = function
  | '.' -> Empty
  | '#' -> Galaxy
  | _ -> failwith "invalid input"
;;

(* turn a string (input line) into a list of cells *)
let parse_line l = l |> String.to_seq |> Seq.map cell_of_char |> List.of_seq

let galaxies_of_input lines =
  let line_len = String.length (List.hd lines) in
  lines
  |> List.map parse_line
  |> List.flatten
  |> List.mapi (fun i c -> (i mod line_len, i / line_len), c)
  |> List.filter_map (fun (p, c) -> if c = Galaxy then Some p else None)
  |> Galaxies.of_list
;;

(* end of parsing section *)

let get_empties map lines =
  let no_cols = String.length (List.hd lines) in
  let no_rows = List.length lines in
  let all_pos_cols = List.init no_cols (fun i -> i, 0) |> Galaxies.of_list in
  let all_pos_rows = List.init no_rows (fun i -> 0, i) |> Galaxies.of_list in
  let empty_rows =
    map |> Galaxies.map (fun (_, y) -> 0, y) |> Galaxies.diff all_pos_rows
  in
  let empty_cols =
    map |> Galaxies.map (fun (x, _) -> x, 0) |> Galaxies.diff all_pos_cols
  in
  Galaxies.union empty_rows empty_cols
;;

let common empty_val =
  let input_lines = Utils.read_input input_file in
  (* get set with all the galaxies *)
  let galaxies = galaxies_of_input input_lines in
  (* get a tuple with the empty rows and columns *)
  let empties = get_empties galaxies input_lines in
  (* calulate the sum of all minimum distances between galaxies *)
  galaxies |> add_all_distances empties empty_val
;;

(* solution for the part1 *)
let part1 = common 2
let () = assert (part1 = 9723824)

(* solution for the part2 *)
let part2 = common 1000000
let () = assert (part2 = 731244261352)
