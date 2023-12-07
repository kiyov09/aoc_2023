let input_test = "./inputs/day5/test.txt"
let input_file = "./inputs/day5/input.txt"

(* return the 2nd element of a list. Useful to get the right side of a split
   result *)
let snd_el lst = List.hd (List.tl lst)

(* parse the seeds line *)
let parse_seeds line =
  line |> String.split_on_char ':' |> snd_el |> Utils.split_ws |> List.map int_of_string
;;

(* turn a string consisting of space separated numbers into a list of ints *)
let parse_line_of_nums line = line |> Utils.split_ws |> List.map int_of_string

(* turn a list of exactly 3 ints into a (src, (dst, count)) *)
let map_binding_of_list lst =
  match lst with
  | [ dst; src; count ] -> [ src, (dst, count) ]
  | _ -> failwith "wrong input"
;;

(* a map from int (src) to (int (dst), int (count)) *)
module Int_to_pair = Map.Make (Int)

(* parse a block of the input file and turn it into an Int_to_pair map *)
let parse_block = function
  | [] -> Int_to_pair.empty
  | _ :: tl ->
    tl
    |> List.map parse_line_of_nums
    |> List.map map_binding_of_list
    |> List.flatten
    |> List.to_seq
    |> Int_to_pair.of_seq
;;

(* parse the input file *)

(* get the next block from the input file. A block here is a consecutive list of
   lines, separated by an empty line *)
let get_next_block lst =
  let rec aux acc = function
    | [] ->
      (match acc with
       | [] -> None, []
       | l -> Some (List.rev l), [])
    | hd :: tl -> if hd = "" then Some (List.rev acc), tl else aux (hd :: acc) tl
  in
  aux [] lst
;;

(* unfold the input file into a list of blocks *)
let next_block lst =
  match get_next_block lst with
  | Some l, tl -> Some (l, tl)
  | None, _ -> None
;;

(* get the max possible key in an Int_to_pair map. Because each key in the map is
   the starting point of a range, the max possible key will be the max one + the
   num of element in the range (which is the second element of the key's value) *)
let max_possible_key map =
  Int_to_pair.bindings map
  |> List.map (fun (k, (_, c)) -> k + c)
  |> List.sort Int.compare
  |> List.rev
  |> List.hd
;;

(* get the value associated to a key in an Int_to_pair map.
   If the key is not in the map:
   - if the key is smaller than the min key in the map, return the key
   - if the key is greater than the max key in the map, return the key
   - otherwise, calculate the value from the biggest key smaller than our key
*)
let get_key_value k map =
  let min = Int_to_pair.min_binding map |> fst in
  let max = max_possible_key map in
  if k < min || k > max
  then k
  else (
    let src, (dts, count) = Int_to_pair.find_last (fun k' -> k' <= k) map in
    let delta = k - src in
    if delta < count then dts + delta else k)
;;

let location_of_seed seed map =
  let rec locate k map =
    match map with
    | [] -> k
    | hd :: tl ->
      if Int_to_pair.is_empty hd then locate k tl else locate (get_key_value k hd) tl
  in
  locate seed map
;;

(* solution for the part 1 *)
let part1 =
  (* let lines = input_test |> Utils.read_input in *)
  let lines = input_file |> Utils.read_input in
  let seeds = parse_seeds (List.hd lines) in
  let map =
    lines
    |> List.tl
    |> Seq.unfold next_block
    |> Seq.map parse_block
    |> List.of_seq
    |> List.filter (fun m -> not (Int_to_pair.is_empty m))
  in
  seeds |> List.map (fun s -> location_of_seed s map) |> List.sort Int.compare |> List.hd
;;

(* ---------- part 2 ---------- *)

(* a range of ints *)
type range =
  { start : int
  ; finish : int
  }

(* return the intersection of 2 ranges *)
let intersection r1 r2 =
  let { start = s1; finish = f1 } = r1 in
  let { start = s2; finish = f2 } = r2 in
  { start = max s1 s2; finish = min f1 f2 }
;;

(* return the union of 2 ranges *)
let union r1 r2 =
  let { start = s1; finish = f1 } = r1 in
  let { start = s2; finish = f2 } = r2 in
  { start = min s1 s2; finish = max f1 f2 }
;;

(* a mapping from a range of ints to another range of ints *)
type mapping =
  { init_k : int
  ; init_v : int
  ; no_of_elements : int
  }

(* return a range of the keys given a mapping *)
let range_of_mappping m =
  let { init_k; no_of_elements; _ } = m in
  { start = init_k; finish = init_k + no_of_elements - 1 }
;;

(* whether 2 ranges overlap (aka: have a non empty intersection) *)
let overlap r1 r2 =
  let inter = intersection r1 r2 in
  inter.start <= inter.finish
;;

(* whether a range contains another range (r1 contains r2) *)
let contains r1 r2 = r1.start <= r2.start && r1.finish >= r2.finish

(* apply the mapping to a range.
   1. If the range is not in the mapping's domain, returns the range unchanged.
   2. If the whole range is in the mapping's domain, returns the range mapped to
   the mapping's codomain.
   3. If the range is partially in the mapping's domain, returns the mapped range
   and the remaining ranges.
*)
let rec map_range m r =
  if not (overlap (range_of_mappping m) r)
  then [ r ]
  else if contains (range_of_mappping m) r
  then (
    let mapped_range =
      { start = m.init_v + (r.start - m.init_k)
      ; finish = m.init_v + (r.finish - m.init_k)
      }
    in
    [ mapped_range ])
  else (
    let inter = intersection (range_of_mappping m) r in
    let mapped_section = map_range m inter in
    let left_section = { start = r.start; finish = inter.start - 1 } in
    let right_section = { start = inter.finish + 1; finish = r.finish } in
    mapped_section @ [ left_section; right_section ]
    |> List.filter (fun r -> r.start <= r.finish))
;;

(* ---- parsing section ---- *)

(* parse the seeds line *)

let range_list_of_string line =
  let make_pairs lst =
    let rec _make acc = function
      | [] -> List.rev acc
      | [ _ ] -> List.rev acc
      | hd :: hd' :: tl -> _make ((hd, hd') :: acc) tl
    in
    _make [] lst
  in
  line
  |> String.split_on_char ':'
  |> snd_el
  |> Utils.split_ws
  |> List.map int_of_string
  |> make_pairs
  |> List.map (fun (r, c) -> { start = r; finish = r + c - 1 })
;;

(* parse a block of the input file and turn it into a list of mappings *)
let parse_block_into_mappings = function
  | [] -> []
  | _ :: tl ->
    tl
    |> List.map parse_line_of_nums
    |> List.map map_binding_of_list
    |> List.flatten
    |> List.to_seq
    |> Int_to_pair.of_seq
    |> Int_to_pair.bindings
    |> List.map (fun (k, (v, c)) -> { init_k = k; init_v = v; no_of_elements = c })
;;

(* unfold the input file into a list of blocks *)
let parse_blocks lines =
  lines
  |> Seq.unfold next_block
  |> Seq.map parse_block_into_mappings
  |> List.of_seq
  |> List.filter (fun m -> List.length m <> 0)
;;

(* apply a list of maps to a range *)
let apply_maps_to_range maps r =
  let maps_that_apply = List.filter (fun m -> overlap (range_of_mappping m) r) maps in
  match maps_that_apply with
  | [] -> [ r ]
  | [ m ] -> map_range m r
  | _ ->
    maps_that_apply
    |> List.map (fun m -> map_range m r)
    (* the first one is the intersection *)
    |> List.map List.hd
;;

(* apply each map sets of maps to each range in the list *)
let apply_maps_to_ranges maps ranges =
  let rec aux ranges = function
    | [] -> ranges
    | hd :: tl ->
      let rs = List.map (fun r -> apply_maps_to_range hd r) ranges |> List.flatten in
      aux rs tl
  in
  aux ranges maps
;;

(* solution for the part 2 *)
let part2 =
  (* let lines = input_test |> Utils.read_input in *)
  let lines = input_file |> Utils.read_input in
  let seeds = range_list_of_string (List.hd lines) in
  let maps = parse_blocks (List.tl lines) in
  seeds
  |> List.map (fun s -> apply_maps_to_ranges maps [ s ])
  |> List.flatten
  |> List.sort (fun r1 r2 -> Int.compare r1.start r2.start)
  |> List.hd
  |> fun n -> n.start
;;
