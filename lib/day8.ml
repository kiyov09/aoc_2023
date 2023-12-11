let input_test = "./inputs/day8/test.txt"
let input_test_2 = "./inputs/day8/test2.txt"
let input_file = "./inputs/day8/input.txt"

type parts =
  { left : string
  ; right : string
  }

let parts_of_string (l, r) = { left = l; right = r }
let left_of_parts { left; _ } = left
let right_of_parts { right; _ } = right

(* parsing *)
let parse_instructions_line l = l |> String.to_seq |> Seq.cycle

let parse_node_line l =
  Scanf.sscanf l "%s = (%s@, %s@)" (fun v l r -> v, parts_of_string (l, r))
;;

module String_map = Map.Make (String)

let map_from_list lst = lst |> List.to_seq |> String_map.of_seq
let make_graph lines = lines |> Utils.skip 1 |> List.map parse_node_line |> map_from_list

(* solutions *)

let navigate inst graph start stop_fn =
  let rec _aux inst acc curr =
    if stop_fn curr
    then acc
    else (
      let curr_info = String_map.find curr graph in
      match inst () with
      | Seq.Cons ('L', inst) -> _aux inst (acc + 1) curr_info.left
      | Seq.Cons ('R', inst) -> _aux inst (acc + 1) curr_info.right
      | _ -> failwith "invalid instruction")
  in
  _aux inst 0 start
;;

let part1 =
  let input = Utils.read_input input_file in
  (* let input = Utils.read_input input_test in *)
  let inst = input |> List.hd |> parse_instructions_line in
  let graph = input |> List.tl |> make_graph in
  navigate inst graph "AAA" (( = ) "ZZZ")
;;

let () = assert (part1 = 17263)

(* part two specifics *)
let ends_with c line = String.ends_with ~suffix:(Char.escaped c) line
let ends_with_a = ends_with 'A'
let ends_with_z = ends_with 'Z'

(** least common multiple *)
let lcm lst =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  let rec _aux acc = function
    | [] -> acc
    | h :: t -> _aux (acc * h / gcd acc h) t
  in
  _aux 1 lst
;;

let navigate' inst graph =
  graph
  |> String_map.bindings
  |> List.map fst
  |> List.filter ends_with_a
  |> List.map (fun a -> navigate inst graph a ends_with_z)
  |> lcm
;;

(* part two *)
let part2 =
  let input = Utils.read_input input_file in
  (* let input = Utils.read_input input_test_2 in *)
  let inst = input |> List.hd |> parse_instructions_line in
  let graph = input |> List.tl |> make_graph in
  navigate' inst graph
;;

let () = assert (part2 = 14631604759649)
