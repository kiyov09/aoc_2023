let input_test = "./inputs/day17/test.txt"

(* let input_file = "./inputs/day17/test.txt" *)
let input_file = "./inputs/day17/input.txt"

(* direction we are facing *)
type dir =
  | Up
  | Down
  | Left
  | Right

let turn_left = function
  | Up -> Left
  | Down -> Right
  | Left -> Down
  | Right -> Up
;;

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down
;;

(* a point in the map *)
type pos = int * int

let move (x, y) = function
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Left -> x - 1, y
  | Right -> x + 1, y
;;

(* the current state we are analyzing
   - pos: our current position
   - dir: the direction we are facing
   - steps: We can only move up to 3 blocks forward. After that we
     need to turn 90 degrees on either side.
*)
type state =
  { pos : pos
  ; dir : dir
  ; steps : int
  }

(* get the next states to analyze from the current one *)
let next_states s =
  let forward = { s with pos = move s.pos s.dir; steps = s.steps + 1 } in
  let left = { pos = move s.pos (turn_left s.dir); dir = turn_left s.dir; steps = 1 } in
  let right =
    { pos = move s.pos (turn_right s.dir); dir = turn_right s.dir; steps = 1 }
  in
  match s.steps with
  | 3 -> [| left; right |]
  | _ -> [| forward; left; right |]
;;

module Dists = Hashtbl.Make (struct
    type t = state

    let equal = ( = )
    let hash = Hashtbl.hash
  end)

module Seen = Set.Make (struct
    type t = state

    let compare = compare
  end)

(* get an optional value from the coordinate *)
let get_at_pos (x, y) map =
  try Some map.(y).(x) with
  | _ -> None
;;

let compare_state_cost (s1, c1) (s2, c2) =
  match compare c1 c2 with
  | 0 -> compare s1 s2
  | n -> n
;;

(* to be able to break out of the loop *)
exception Found of int

let find_path next_states (map : int array array) =
  let rows = Array.length map in
  let cols = Array.length map.(0) in
  let is_end (x, y) = x = cols - 1 && y = rows - 1 in

  (* dists *)
  let dists = Dists.create (rows * cols * 4) in
  (* priority queue *)
  let heap = Collections.Heap.create ~cmp:compare_state_cost () in

  (* add all the starting states to the queue *)
  [ { pos = 0, 0; dir = Right; steps = 1 }; { pos = 0, 0; dir = Down; steps = 1 } ]
  |> List.iter (fun s ->
    Collections.Heap.push (s, 0) heap;
    Dists.add dists s 0);

  (* process till there's no more elements in the queue *)
  let find_dist s = Dists.find_opt dists s |> Option.value ~default:max_int in

  try
    while not (Collections.Heap.is_empty heap) do
      (* take the next state from the queue *)
      let curr_state, curr_cost = Collections.Heap.take heap in

      (* if this is the end node, we could ignore it and continue
         processing the queue *)
      if is_end curr_state.pos
      then raise (Found curr_cost)
      else (
        (* get the distance to the node we are currently processing *)
        let dist = find_dist curr_state in

        if curr_cost <= dist
        then
          (* get the next states from the current one *)
          next_states curr_state
          |> Array.iter (fun s ->
            match get_at_pos s.pos map with
            | None -> () (* ignore if we are out of bounds *)
            | Some next_cost ->
              (* calculate the cost for the next state *)
              let next_cost = curr_cost + next_cost in

              (* get the distance to the next state *)
              let dist_to_next = find_dist s in

              (* if the cost to the next state is less than the distance
                 we have seen so far, then we need to update the distance
                 and add the next state to the queue *)
              if next_cost < dist_to_next
              then (
                Dists.replace dists s next_cost;
                Collections.Heap.push (s, next_cost) heap)))
    done;
    None
  with
  | Found n -> Some n
;;

(* create the map (2d matrix) from the input *)
let create_map lines =
  let rows = List.length lines in
  let cols = List.hd lines |> String.length in
  let map = Array.make_matrix rows cols 0 in
  List.iteri
    (fun y line ->
      String.iteri
        (fun x c -> map.(y).(x) <- Utils.int_of_digit_char c |> Option.get)
        line)
    lines;
  map
;;

(* common code for both parts *)
let common states_fn =
  (* Utils.read_input input_test *)
  Utils.read_input input_file |> create_map |> find_path states_fn |> Option.get
;;

(* solution for the part 1 *)
let part1 = common next_states
let () = assert (part1 = 785)

let next_states' s =
  (* turn left and move one step *)
  let left = { pos = move s.pos (turn_left s.dir); dir = turn_left s.dir; steps = 1 } in
  (* turn right and move one step *)
  let right =
    { pos = move s.pos (turn_right s.dir); dir = turn_right s.dir; steps = 1 }
  in
  (* move forward *)
  let forward = { s with pos = move s.pos s.dir; steps = s.steps + 1 } in
  (* based on how forward we have moved *)
  match s.steps with
  (* we must keep moving forward if not get to 4 consecutive steps yet *)
  | n when n < 4 -> [| forward |]
  (* if we moved 10 or more steps forward, we must turn left or right *)
  | n when n >= 10 -> [| left; right |]
  (* otherwise we can move forward or turn left or right *)
  | _ -> [| forward; left; right |]
;;

(* solution for the part 2 *)
let part2 = common next_states'
let () = assert (part2 = 922)
