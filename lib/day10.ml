let input_test = "./inputs/day10/test.txt"
let input_test_2 = "./inputs/day10/test2.txt"
let input_file = "./inputs/day10/input.txt"

type pos = int * int

(* a map with pos as keys *)
module Map_pos = Map.Make (struct
    type t = pos

    let compare = compare
  end)

type ttype =
  | Vertical
  | Horizontal
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight

(* turn a char into a tile type *)
let ttype_of_char_opt = function
  | '|' -> Some Vertical
  | '-' -> Some Horizontal
  | 'F' -> Some TopLeft
  | '7' -> Some TopRight
  | 'L' -> Some BottomLeft
  | 'J' -> Some BottomRight
  | _ -> None
;;

type tile =
  | Start
  | Tile of ttype
  | Inside

(* turn a char into a tile *)
let tile_of_char_opt = function
  | '.' -> None
  | 'S' -> Some Start
  | c -> ttype_of_char_opt c |> Option.map (fun tt -> Tile tt)
;;

(* deltas to move to the next tile:
   down, right, up, left *)
let deltas = [ 0, -1; 1, 0; 0, 1; -1, 0 ]

(* add two positions *)
let add_pos (x1, y1) (x2, y2) = x1 + x2, y1 + y2

(* subtract two positions *)
let sub_pos (x1, y1) (x2, y2) = x1 - x2, y1 - y2

(* datermine if the 2nd argument is a valid next tile for the 1st one *)
let valid_next_tile (curr_pos, curr_tile) (tile_pos, tile) =
  let delta = sub_pos tile_pos curr_pos in
  match curr_tile, tile, delta with
  (* to the left only Horizontal or *Left tiles are valid *)
  | Start, Tile Vertical, (-1, 0) -> false
  | Start, Tile TopRight, (-1, 0) -> false
  | Start, Tile BottomRight, (-1, 0) -> false
  (* to the right only Horizontal or *Right tiles are valid *)
  | Start, Tile Vertical, (1, 0) -> false
  | Start, Tile TopLeft, (1, 0) -> false
  | Start, Tile BottomLeft, (1, 0) -> false
  (* to the top only Vertical or *Top tiles are valid *)
  | Start, Tile Horizontal, (0, -1) -> false
  | Start, Tile BottomLeft, (0, -1) -> false
  | Start, Tile BottomRight, (0, -1) -> false
  (* to the bottom only Vertical or *Bottom tiles are valid *)
  | Start, Tile Horizontal, (0, 1) -> false
  | Start, Tile TopRight, (0, 1) -> false
  | Tile curr, Tile tile, _ ->
    (match curr, tile, delta with
     (* if curr is Vertical, valid tiles can only be up or down, and none of them
        can be Horizontal *)
     | Vertical, Horizontal, _ -> false
     | Vertical, _, (1, 0) -> false
     | Vertical, _, (-1, 0) -> false
     (* if curr is Horizontal, valid tiles can only be left or right, and none of them
        can be Vertical *)
     | Horizontal, Vertical, _ -> false
     | Horizontal, _, (0, 1) -> false
     | Horizontal, _, (0, -1) -> false
     (* if curr is TopLeft, valid tiles can only be to the right or below, and
        the one to the right can't be vertical *)
     | TopLeft, _, (-1, 0) -> false
     | TopLeft, _, (0, -1) -> false
     | TopLeft, Vertical, (1, 0) -> false
     (* if curr is TopRight, valid tiles can only be to the left or below, and
        the one to the left can't be vertical *)
     | TopRight, _, (1, 0) -> false
     | TopRight, _, (0, -1) -> false
     | TopRight, Vertical, (-1, 0) -> false
     (* if curr is BottomLeft, valid tiles can only be to the right or above, and
        the one to the right can't be vertical *)
     | BottomLeft, _, (-1, 0) -> false
     | BottomLeft, _, (0, 1) -> false
     | BottomLeft, Vertical, (1, 0) -> false
     (* if curr is BottomRight, valid tiles can only be to the left or above, and
        the one to the left can't be vertical *)
     | BottomRight, _, (1, 0) -> false
     | BottomRight, _, (0, 1) -> false
     | BottomRight, Vertical, (-1, 0) -> false
     | _ -> true)
  | _ -> true
;;

let solve map start =
  (* get the next valid tiles from the current one *)
  let next_tiles prev =
    deltas
    |> List.map (add_pos (fst prev))
    |> List.filter_map (fun p -> map |> Map_pos.find_opt p |> Option.map (fun t -> p, t))
    |> List.filter (fun tile -> valid_next_tile prev tile)
  in
  (* move to the next "tile" till the branches reach each other *)
  let rec _aux prev_l left prev_r right steps =
    match left, right with
    | (pos, _), (pos', _) when pos = pos' -> steps
    | _ ->
      let next_left =
        next_tiles left |> List.filter (fun (p, _) -> p <> prev_l) |> List.hd
      in
      let next_right =
        next_tiles right |> List.filter (fun (p, _) -> p <> prev_r) |> List.hd
      in
      _aux (fst left) next_left (fst right) next_right (steps + 1)
  in
  (* get the two possible starts and start the recursion *)
  let starts = next_tiles start in
  let start_pos = fst start in
  (* it's granted that there are only two possible starts *)
  _aux start_pos (List.nth starts 0) start_pos (List.nth starts 1) 1
;;

(* parsing input section *)
let parse_input lines =
  lines
  (* turn each line into a sequence of chars *)
  |> List.map String.to_seq
  (* turn each char in the sequence into a pos * tile *)
  |> List.mapi (fun y line ->
    line
    |> Seq.mapi (fun x c -> (x, y), tile_of_char_opt c)
    |> Seq.filter_map (fun (pos, t) -> t |> Option.map (fun t -> pos, t)))
  (* turn the sequence of pos * tile into a list of pos * tile *)
  |> List.map List.of_seq
  |> List.flatten
  (* make it a seq again to build the map *)
  |> List.to_seq
  |> Map_pos.of_seq
;;

(* solution for the part 1 *)
let part1 =
  (* let map = parse_input (Utils.read_input input_test) in *)
  let map = parse_input (Utils.read_input input_file) in
  let start = map |> Map_pos.bindings |> List.find (fun (_, t) -> t = Start) in
  solve map start
;;

let () = assert (part1 = 6956)

let get_loop_tiles map start =
  (* get the next valid tiles from the current one *)
  let next_tiles prev =
    deltas
    |> List.map (add_pos (fst prev))
    |> List.filter_map (fun p -> map |> Map_pos.find_opt p |> Option.map (fun t -> p, t))
    |> List.filter (fun tile -> valid_next_tile prev tile)
  in
  (* move to the next "tile" till the branches reach each other *)
  let rec _aux prev_l left prev_r right result =
    let result = prev_l :: fst result, prev_r :: snd result in
    match left, right with
    | (pos, _), (pos', _) when pos = pos' ->
      (* List.tl (List.rev (fst result)) @ [ pos ] @ snd result *)
      [ pos ] @ snd result @ List.tl (List.rev (fst result))
    | _ ->
      let next_left =
        next_tiles left |> List.filter (fun (p, _) -> p <> prev_l) |> List.hd
      in
      let next_right =
        next_tiles right |> List.filter (fun (p, _) -> p <> prev_r) |> List.hd
      in
      _aux (fst left) next_left (fst right) next_right result
  in
  (* get the two possible starts and start the recursion *)
  let starts = next_tiles start in
  let start_pos = fst start in
  (* it's granted that there are only two possible starts *)
  _aux start_pos (List.nth starts 0) start_pos (List.nth starts 1) ([], [])
;;

(* shoelace formula to calculate the area of a polygon
   https://en.wikipedia.org/wiki/Shoelace_formula
*)
let shoelace_formula points =
  let _2_by_2_det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1) in
  let first = List.hd points in
  let rec _aux acc = function
    | [] -> failwith "unreachable!"
    | [ last ] -> acc + _2_by_2_det last first
    | p :: (p' :: _ as ps) -> _aux (acc + _2_by_2_det p p') ps
  in
  let sum = _aux 0 points in
  abs (sum / 2)
;;

(* solution for the part 2 *)
let part2 =
  let input = Utils.read_input input_file in
  (* let input = Utils.read_input input_test_2 in *)
  let map = parse_input input in
  let start = map |> Map_pos.bindings |> List.find (fun (_, t) -> t = Start) in
  (* get the loop tiles *)
  let loop = get_loop_tiles map start in
  let polygon_area = shoelace_formula loop in
  (* number of tiles in the loop *)
  let loop_length = List.length loop in
  (*
     According to Pick's theorem, the area of a polygon on a grid can be calculated
     by the following formula:
     A = i + b/2 - 1
     where:
     - A is the area of the polygon
     - i is the number of interior points with integer coordinates
     - b is the number of boundary points with integer coordinates

     so, if we know the area and the number of boundary points, we can calculate
     the number of interior points:
     i = A - b/2 + 1
  *)
  polygon_area - (loop_length / 2) + 1
;;

let () = assert (part2 = 455)
