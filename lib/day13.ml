(*
   credits to David Brownman ( xavdid )
   https://advent-of-code.xavd.id/writeups/2023/day/13/

   I had almost the same idea, but I was able to adapt it to part 2 properly.
*)

let input_test = "./inputs/day13/test.txt"
let input_file = "./inputs/day13/input.txt"

(* parsing *)

(* split the whole input into each chunk *)
let group_input input =
  let rec _aux acc = function
    (* end of the input *)
    | [] -> acc |> List.rev |> List.map List.rev
    (* still input *)
    | hd :: tl ->
      if hd = "" (* when we get an empty line start a new group *)
      then _aux ([] :: acc) tl
      else (
        (* otherwise add the line to the current group *)
        match acc with
        | [] -> _aux ([ hd ] :: acc) tl
        | hd' :: tl' -> _aux ((hd :: hd') :: tl') tl)
  in
  _aux [] input
;;

(* end parsing *)

(* we have a list of strings, which can be seen as a matrix of chars.
   this function will rotate the matrix to the right. *)
let rotate_rigth lst =
  let max = lst |> List.hd |> String.length in
  let rec _iter acc i =
    if i = max
    then acc |> List.rev |> List.map (fun s -> s |> List.to_seq |> String.of_seq)
    else (
      let rec _iter' acc' = function
        | [] -> _iter (acc' :: acc) (i + 1)
        | hd :: tl -> _iter' (String.get hd i :: acc') tl
      in
      _iter' [] lst)
  in
  _iter [] 0
;;

(* the response (at least for part 1) is `total_cols + 100 * total_rows` *)
let calculate_response (rows, cols) =
  let cols = Utils.sum cols in
  let rows = Utils.sum rows in
  cols + (100 * rows)
;;

(* count the number of different chars between two strings *)
let diff_count s s' =
  Seq.fold_left2
    (fun acc c c' -> if c = c' then acc else acc + 1)
    0
    (String.to_seq s)
    (String.to_seq s')
;;

let till_idx idx lst = lst |> List.to_seq |> Seq.take idx
let from_idx idx lst = lst |> List.to_seq |> Seq.drop idx

(* find the reflection line *)
let find_reflection_line lst diff =
  let _aux idx =
    let left = till_idx idx lst |> List.of_seq |> List.rev |> List.to_seq in
    let right = from_idx idx lst in
    Seq.fold_left2 (fun acc l r -> acc + diff_count l r) 0 left right = diff
  in
  lst |> List.tl |> List.mapi (fun i _ -> i + 1) |> List.find_opt _aux
;;

(* process a group returning an Either, where Left means that the reflection
   line is a row, and Right means that the reflection line is a column *)
let process diff lst =
  (* find the reflection in the rows *)
  let rows = find_reflection_line lst diff in
  (* find the reflection in the columns *)
  let cols = find_reflection_line (rotate_rigth lst) diff in
  (* return the result as an Either *)
  match rows, cols with
  | Some row, _ -> Either.left row
  | _, Some col -> Either.right col
  | _ -> failwith "invalid input, every group should have at least one reflection line"
;;

let common diff =
  (* Utils.read_input input_test *)
  Utils.read_input input_file
  (* turn the input into a list of groups (each group is a list of strings) *)
  |> group_input
  (* process each group obtaining a list of Either, where Left means that the reflection
     line is a row, and Right means that the reflection line is a column *)
  |> List.map (process diff)
  (* partition the list resulting on a tuple of two lists, the first one containing
     all the Left values, and the second one containing all the Right values *)
  |> List.partition_map Fun.id
  (* calculate the response by applying the formula: `total_cols + 100 * total_rows` *)
  |> calculate_response
;;

(* solution for the part 1 *)
let part1 = common 0
let () = assert (part1 = 33728)

(* solution for the part 2 *)
let part2 = common 1
let () = assert (part2 = 28235)
