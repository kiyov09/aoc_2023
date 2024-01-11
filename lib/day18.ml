let input_test = "./inputs/day18/test.txt"
let input_file = "./inputs/day18/input.txt"

type point = int * int

type dir =
  | Up
  | Down
  | Left
  | Right

let dir_of_char = function
  | 'R' | '0' -> Right
  | 'D' | '1' -> Down
  | 'L' | '2' -> Left
  | 'U' | '3' -> Up
  | _ -> failwith "invalid char"
;;

type command =
  { dir : dir
  ; steps : int
  }

let command_of_input_line (l : string) =
  Scanf.sscanf l "%c %d (#%s@)" (fun d s _ -> { dir = dir_of_char d; steps = s })
;;

let next_point ((x, y) : point) ({ dir; steps; _ } : command) =
  match dir with
  | Up -> x, y - steps
  | Down -> x, y + steps
  | Left -> x - steps, y
  | Right -> x + steps, y
;;

let all_in_between p dir steps =
  let rec loop acc = function
    | 0 -> acc
    | n ->
      let x', y' = next_point p { dir; steps = n } in
      loop ((x', y') :: acc) (n - 1)
  in
  loop [] steps
;;

let points_of_input (f : string -> command) (input : string list) =
  let total_points = ref 0 in

  let fold_fn acc curr =
    match acc with
    (* | prev :: _ -> List.rev (all_in_between prev curr.dir curr.steps) @ acc *)
    | prev :: _ ->
      total_points := !total_points + curr.steps;
      next_point prev curr :: acc
    | [] -> failwith "unreachable!"
  in

  let points = input |> List.map f |> List.fold_left fold_fn [ 0, 0 ] |> List.tl in
  points, !total_points
;;

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

let polygon_area (points, len) =
  (* let len = points |> List.map snd |> List.fold_left ( + ) 1 in *)
  let shoelace_area = points |> shoelace_formula in
  (* pick's theorem *)
  let area_inside = shoelace_area - (len / 2) + 1 in

  area_inside + len
;;

let common fn = Utils.read_input input_file |> points_of_input fn |> polygon_area

(* solution of the part 1 *)
let part1 = common command_of_input_line
let () = assert (part1 = 61661)

let command_of_input_line' (l : string) =
  let hex_of_string s = int_of_string ("0x" ^ s) in
  Scanf.sscanf l "%c %d (#%5[0-9a-f]%c)" (fun _ _ steps dir ->
    { dir = dir_of_char dir; steps = hex_of_string steps })
;;

let part2 = common command_of_input_line'
let () = assert (part2 = 111131796939729)
