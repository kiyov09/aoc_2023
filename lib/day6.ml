let input_test = "./inputs/day6/test.txt"
let input_file = "./inputs/day6/input.txt"

(* --- parsing --- *)

(* return the 2nd element of a list. Useful to get the right side of a split
   result *)
let snd_el lst = List.hd (List.tl lst)

(* turn a string consisting of space separated numbers into a list of ints *)
let parse_line_of_nums line = line |> Utils.split_ws |> List.map int_of_string

(* turn a list of ints into a single int *)
let combine_into_big_num lst =
  lst |> List.map string_of_int |> String.concat "" |> int_of_string
;;

(* split by the first colon, then split the right side by spaces and turn the
   result into a list of ints *)
let parse_line line =
  line |> String.split_on_char ':' |> snd_el |> Utils.split_ws |> List.map int_of_string
;;

let parse_line' line =
  line
  |> String.split_on_char ':'
  |> snd_el
  |> Utils.split_ws
  |> List.map int_of_string
  |> combine_into_big_num
;;

(* --- end parsing --- *)

(* --- data --- *)

type race =
  { time : int
  ; dist : int
  }

(* build a race from a pair of ints *)
let race_of_pair (time, dist) = { time; dist }

(* build a list of races from the input lines *)
let race_list_of_input_lines = function
  | [ times; dists ] ->
    List.combine (parse_line times) (parse_line dists) |> List.map race_of_pair
  | _ -> failwith "invalid input"
;;

(* build one race from the input lines *)
let race_of_input_lines = function
  | [ times; dists ] -> race_of_pair (parse_line' times, parse_line' dists)
  | _ -> failwith "invalid input"
;;

(* from a race, calculate in how many ways we can beat the distance *)
let calculate r =
  List.init r.time (fun i -> (r.time - i) * i)
  |> List.filter (( < ) r.dist)
  |> List.length
;;

(* from a list of races, calculate the total number of ways we can beat the
   distance on each one and then multiply them all together *)
let calculate_all races = races |> List.map calculate |> List.fold_left ( * ) 1

let part1 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file |> race_list_of_input_lines |> calculate_all
;;

let part2 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file |> race_of_input_lines |> calculate
;;
