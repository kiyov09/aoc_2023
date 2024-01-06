let input_test = "./inputs/day16/test.txt"
let input_file = "./inputs/day16/input.txt"

type tile =
  | Empty
  | Vertical
  | Horizontal
  | RightM
  | LeftM

let tile_of_char = function
  | '.' -> Empty
  | '|' -> Vertical
  | '-' -> Horizontal
  | '/' -> RightM
  | '\\' -> LeftM
  | _ -> failwith "invalid char"
;;

type direction =
  | Up
  | Down
  | Left
  | Right

let next_dirs dir = function
  | Empty -> [ dir ]
  | Vertical ->
    (match dir with
     | Right | Left -> [ Up; Down ]
     | dir -> [ dir ])
  | Horizontal ->
    (match dir with
     | Up | Down -> [ Left; Right ]
     | dir -> [ dir ])
  | RightM ->
    (match dir with
     | Up -> [ Right ]
     | Down -> [ Left ]
     | Left -> [ Down ]
     | Right -> [ Up ])
  | LeftM ->
    (match dir with
     | Up -> [ Left ]
     | Down -> [ Right ]
     | Left -> [ Up ]
     | Right -> [ Down ])
;;

let matrix_of_input lines =
  let rows = List.length lines in
  let cols = String.length (List.hd lines) in
  let matrix = Array.make_matrix rows cols Empty in
  List.iteri
    (fun row line ->
      String.iteri (fun col c -> matrix.(row).(col) <- tile_of_char c) line)
    lines;
  matrix
;;

type cursor =
  { x : int
  ; y : int
  ; dir : direction
  }

let next_point = function
  | { x; y; dir = Up } -> x, y - 1
  | { x; y; dir = Down } -> x, y + 1
  | { x; y; dir = Left } -> x - 1, y
  | { x; y; dir = Right } -> x + 1, y
;;

let get_at_cursor m c =
  try Some m.(c.y).(c.x) with
  | _ -> None
;;

module CursorSet = Set.Make (struct
    type t = cursor

    let compare = compare
  end)

let process ?(init = { x = -1; y = 0; dir = Right }) map =
  let energized = ref CursorSet.empty in
  let rec visit_tile c =
    (* mark the current pos as energized *)
    energized := CursorSet.add c !energized;
    (* get the next point to move to based on the current direction *)
    let x', y' = next_point c in
    (* get the tile at the next point *)
    match get_at_cursor map { x = x'; y = y'; dir = c.dir } with
    (* None means we are out of the map, so we stop *)
    | None -> ()
    (* if there's a tile ...*)
    | Some tile ->
      (* ... we get the possible directions to move to from this tile *)
      let dirs = next_dirs c.dir tile in
      (* and we move on all of them *)
      dirs
      |> List.map (fun dir -> { x = x'; y = y'; dir })
      |> List.filter (fun c' -> not (CursorSet.mem c' !energized))
      |> List.iter (fun c' -> visit_tile c')
  in
  visit_tile init;
  (* calculate the number of energized tiles (the set is already uniq *)
  !energized
  (* clear any outside the map tile *)
  |> CursorSet.filter (fun c -> get_at_cursor map c <> None)
  (* make all the same dir, effectively (x,y) duplicates *)
  |> CursorSet.map (fun c -> { c with dir = Up })
  |> CursorSet.cardinal
;;

let part1 = Utils.read_input input_file |> matrix_of_input |> process
let () = assert (part1 = 7060)

let process' map =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  (* all horizontal starting points *)
  let rs =
    List.init rows (fun y -> [ { x = -1; y; dir = Right }; { x = cols; y; dir = Left } ])
    |> List.flatten
  in
  (* all vertical starting points *)
  let cs =
    List.init cols (fun x -> [ { x; y = -1; dir = Down }; { x; y = rows; dir = Up } ])
    |> List.flatten
  in
  (* process starting from all the points and get the max energized tiles *)
  rs @ cs
  |> List.map (fun c -> process ~init:c map)
  |> List.fast_sort (fun a b -> compare b a)
  |> List.hd
;;

(* solution for the part2 *)
let part2 = Utils.read_input input_file |> matrix_of_input |> process'
let () = assert (part2 = 7493)
