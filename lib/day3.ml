let input_test = "./inputs/day3/test.txt"
let input_file = "./inputs/day3/input.txt"

(** [loc] is the location of an item in the schematic *)
type loc =
  { line : int
  ; s : int (* start *)
  ; e : int (* end *)
  }

(** [item] is a part of the schematic *)
type item =
  | Num of int * loc
  | Sym of char * loc

(** [int_of_num] returns the integer value of an item if it's a [Num], otherwise
    it fails *)
let int_of_num = function
  | Num (n, _) -> n
  | Sym _ -> failwith "not a number"
;;

(** [item_loc] returns the location of an item *)
let item_loc = function
  | Num (_, loc) -> loc
  | Sym (_, loc) -> loc
;;

(** add a new digit to the end of a number *)
let add_digit_to_num num i = (num * 10) + i

(** [are_adjacent a b] returns true if [a] and [b] are adjacent in the schematic *)
let are_adjacent a b =
  let { line; s; e } = a in
  let { line = line'; s = s'; e = e' } = b in
  let line_diff = abs (line - line') in
  match line_diff with
  (* if they are in the same line, then it needs to be one right after the other *)
  | 0 -> abs (s - e') = 1 || abs (s' - e) = 1
  (* todo: write comment *)
  | 1 -> abs (s - e') <= 1 || abs (s' - e) <= 1
  | _ -> false
;;

(** [get_next_item l pos acc lst] returns the next item in the schematic, plus a
    couple of other values that enable the function to continue from where it left *)
let rec get_next_item l pos acc = function
  (* empty list means no more to get from *)
  | [] ->
    (match acc with
     (* if no building a Num, then return None *)
     | None -> None
     (* else, finish building the num and return it *)
     | Some n ->
       let loc' = { line = l; s = pos - String.length n; e = pos - 1 } in
       Some (Num (int_of_string n, loc'), pos + 1, []))
  (* if found a don't *)
  | '.' :: tl ->
    (match acc with
     (* jump to the next item if no in the middle of building a Num *)
     | None -> get_next_item l (pos + 1) acc tl
     (* if building a Num, define its location and return it *)
     | Some n ->
       let loc' = { line = l; s = pos - String.length n; e = pos - 1 } in
       Some (Num (int_of_string n, loc'), pos + 1, tl))
  (* if digit, start/continue building a num *)
  | hd :: tl when Utils.is_digit hd ->
    let new_acc = Some (Option.value ~default:"" acc ^ Char.escaped hd) in
    get_next_item l (pos + 1) new_acc tl
  (* if here, then it's a symbol *)
  | hd :: tl ->
    (match acc with
     (* if no building a Num, then return the symbol *)
     | None -> Some (Sym (hd, { line = l; s = pos; e = pos }), pos + 1, tl)
     (* else, finish building the num and return it *)
     | Some n ->
       let loc' = { line = l; s = pos - String.length n; e = pos - 1 } in
       Some (Num (int_of_string n, loc'), pos, hd :: tl))
;;

(** [next_item line_no (pos, lst)] returns the next item in the schematic, plus a
    couple of other values that enable the function to continue from where it left *)
let next_item line_no (pos, lst) =
  match get_next_item line_no pos None lst with
  | Some (i, next_pos, lt) -> Some (i, (next_pos, lt))
  | None -> None
;;

(** [get_parts_ids (nums, syms)] returns the ids of the parts that are adjacent
    to at least one of the symbols *)
let get_parts_ids (nums, syms) =
  nums
  |> List.filter (fun num ->
    syms
    |> List.find_opt (fun sym -> are_adjacent (item_loc num) (item_loc sym))
    |> Option.is_some)
  |> List.map (fun num -> int_of_num num)
;;

(** [is_possible_gear gear] returns true if the item passed is a gear (the char
    of the symbol is an '*') *)
let is_possible_gear = function
  | Sym (s, _) -> s = '*'
  | _ -> false
;;

(** [get_ratio_opt gear nums] returns the multiplication of the two numbers
    adjacent to the gear, if there are two, otherwise it returns None *)
let get_ratio_opt gear nums =
  let gear_loc = item_loc gear in
  let adjs = List.filter (fun num -> are_adjacent (item_loc num) gear_loc) nums in
  if List.length adjs = 2
  then adjs |> List.map int_of_num |> List.fold_left ( * ) 1 |> Option.some
  else None
;;

(** [get_ratios (nums, syms)] returns the sum of the ratios of all the gears *)
let get_ratios (nums, syms) =
  syms
  |> List.filter is_possible_gear
  |> List.filter_map (fun gear -> get_ratio_opt gear nums)
;;

(** [common file] is the common part of the solutions of both parts *)
let common file =
  Utils.read_input file
  |> List.mapi (fun i s -> i, (0, Utils.char_list_of_string s))
  |> List.map (fun (line_no, line) -> Seq.unfold (next_item line_no) line |> List.of_seq)
  |> List.flatten
  |> List.partition (function
    | Num _ -> true
    | Sym _ -> false)
;;

(** solution of the part 1 *)
let part1 =
  (* Utils.read_input input_test *)
  common input_file |> get_parts_ids |> List.fold_left ( + ) 0
;;

(** solution of the part 2 *)
let part2 =
  (* Utils.read_input input_test *)
  common input_file |> get_ratios |> List.fold_left ( + ) 0
;;
