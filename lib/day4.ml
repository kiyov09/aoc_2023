let input_test = "./inputs/day4/test.txt"
let input_test_2 = "./inputs/day4/test2.txt"
let input_file = "./inputs/day4/input.txt"

(* just double the input *)
let double = ( * ) 2

(* apply fn n times *)
let rec n_times fn n x = if n = 0 then x else n_times fn (n - 1) (fn x)

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

(* scratchcard *)
type card =
  { winning : IntSet.t
  ; mine : IntSet.t
  }

(* build an empty card, which is a card with no winning numbers and no mine numbers *)
let empty_card = { winning = IntSet.empty; mine = IntSet.empty }

(* the number of matching numbers is the intersection of the winning numbers and the mine numbers *)
let no_of_matching c =
  let { winning; mine } = c in
  IntSet.inter winning mine |> IntSet.elements |> List.length
;;

(* the points of a card is 0 if there are no matching numbers, otherwise is the
   result of starting from 1 and doubling it n-1 times, where n is the number of matching numbers *)
let points_of_card c =
  let n = no_of_matching c in
  if n = 0 then 0 else n_times double (n - 1) 1
;;

(* parsing section *)

(* make a set from a string of numbers separated by spaces *)
let make_set s =
  s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> IntSet.of_list
;;

(* accumulate the sets in a card *)
let acc_card acc n =
  if IntSet.is_empty acc.winning then { acc with winning = n } else { acc with mine = n }
;;

(* process a line of the input file, giving a card as a result *)
let process_card_line line =
  let reverse_nth n lst = List.nth lst n in
  line
  |> String.split_on_char ':'
  |> reverse_nth 1
  |> String.split_on_char '|'
  |> List.map make_set
  |> List.fold_left acc_card empty_card
;;

(* end parsing section *)

(* common part of the two parts *)
let common file = Utils.read_input file |> List.map process_card_line

(* solution of the part 1 *)
(* process each line of the input to get a card, then get the points of each card
   and sum them all *)
let part1 = common input_file |> List.map points_of_card |> List.fold_left ( + ) 0

(* increase the value of the cards with id from id+1 to id+n by the value of the card with id *)
let increase_following_n_cards map (id, n) =
  (* the ids of the cards we need to increase *)
  let ids_to_incr = List.init n (fun i -> id + i + 1) in
  (* should not fail cuz we created the map already *)
  let val_of_id = IntMap.find id map in
  (* update each of the ids with its current value + the value of the card *)
  List.fold_left
    (* the key we are updating is granted to be in the map so Option.map is safe
       here*)
      (fun acc id -> IntMap.update id (Option.map (( + ) val_of_id)) acc)
    map
    ids_to_incr
;;

(* creates a map with keys from 1 to n, all with the value of 1 *)
let map_of_int n = List.init n (fun i -> i + 1, 1) |> List.to_seq |> IntMap.of_seq

(* fold the values of a map by using fn and init as the initial value *)
let fold_map_values fn init map =
  IntMap.bindings map |> List.map snd |> List.fold_left fn init
;;

(* solution of the part 2 *)
let part2 =
  let with_ids =
    (* Utils.read_input input_test_2 *)
    common input_file |> List.map no_of_matching |> List.mapi (fun i n -> i + 1, n)
  in
  let init_map = List.length with_ids |> map_of_int in
  with_ids
  |> List.fold_left increase_following_n_cards init_map
  |> fold_map_values ( + ) 0
;;
