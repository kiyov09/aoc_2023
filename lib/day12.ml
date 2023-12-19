(*
   credits to David Brownman ( xavdid )
   https://github.com/xavdid/advent-of-code/blob/main/solutions/2023/day_12/solution.py

   I'haven't been able to take part2 time into a reasonable amount of time
   without it's solution
*)

let input_test = "./inputs/day12/test.txt"
let input_file = "./inputs/day12/input.txt"

(* parsing section *)
let parse_comma_sep_nums str = str |> String.split_on_char ',' |> List.map int_of_string

let parse_line line =
  match String.split_on_char ' ' line with
  | [ pattern; groups ] -> pattern, parse_comma_sep_nums groups
  | _ -> failwith "invalid line"
;;

module Memo = Map.Make (struct
    type t = char list * int list

    let compare = compare
  end)

let fit_group p gsize =
  p |> List.to_seq |> Seq.take gsize |> Seq.for_all (fun x -> x <> '.')
  && if List.length p > gsize then List.nth p gsize <> '#' else true
;;

(* skip a group, aka: drop gsize + 1 chars *)
let skip_group p gsize = p |> List.to_seq |> Seq.drop (gsize + 1) |> List.of_seq

let process_line (pattern, groups) =
  (* turn the string into a list of chars *)
  let pattern = pattern |> String.to_seq |> List.of_seq in
  (* memoization (using imperative features) *)
  let memo = ref Memo.empty in
  (* update memoization *)
  let update_memo k v = memo := Memo.add k v !memo in
  (* recursive function to calculate the number of valid solutions *)
  let rec _calculate p gs =
    (* if we already have a result for this pattern and groups, return it *)
    match Memo.find_opt (p, gs) !memo with
    | Some v -> v
    | None ->
      let r =
        match p with
        (* if pattern is done and there are no more groups, this is a valid solution *)
        | [] -> if List.length gs = 0 then 1 else 0
        (* if current char is a dot, just recurse *)
        | hd :: tl when hd = '.' -> _calculate tl gs
        (* if current char is a hash means a new group has started so ... *)
        | hd :: _ when hd = '#' ->
          (match gs with
           (* if there are no more groups, this is not a valid solution *)
           | [] -> 0
           | g :: gs ->
             (* if the group is bigger than the pattern, this is not a valid solution *)
             if List.length p < g
             then
               0
               (* if the group doesn't fit in the pattern, this is not a valid solution *)
             else if not (fit_group p g)
             then 0
             else _calculate (skip_group p g) gs)
        (* at this point, hd is a '?' *)
        | _ :: tl -> _calculate ('.' :: tl) gs + _calculate ('#' :: tl) gs
      in
      (* update memoization *)
      let _ = update_memo (p, gs) r in
      r
  in
  _calculate pattern groups
;;

let common parse_line_fn =
  Utils.read_input input_file
  |> List.map parse_line_fn
  |> List.map process_line
  |> List.fold_left ( + ) 0
;;

(* solution for part 1 *)
let part1 = common parse_line
let () = assert (part1 = 7007)

let parse_line_with_repeat line =
  let pattern, groups = parse_line line in
  let patttern =
    pattern |> Seq.repeat |> Seq.take 5 |> List.of_seq |> String.concat "?"
  in
  let groups = groups |> Seq.repeat |> Seq.take 5 |> List.of_seq |> List.flatten in
  patttern, groups
;;

(* solution for part 2 *)

let part2 = common parse_line_with_repeat
let () = assert (part2 = 3476169006222)
