let input_test = "./inputs/day14/test.txt"
let input_file = "./inputs/day14/input.txt"

let is_cube_shaped_rock = function
  | '#' -> true
  | _ -> false
;;

let swap_till stop_fn lst =
  let first = List.hd lst in
  let tail = List.tl lst in
  let rec _swap_till first acc = function
    | [] -> first :: acc |> List.rev
    | x :: xs when stop_fn x -> List.rev_append (x :: first :: acc) xs
    | x :: xs -> _swap_till first (x :: acc) xs
  in
  _swap_till first [] tail
;;

let make_all_drop str =
  let rec _aux = function
    | hd :: tl when hd <> 'O' -> hd :: _aux tl
    | hd :: tl -> swap_till is_cube_shaped_rock (hd :: _aux tl)
    | l -> l
  in
  _aux (str |> String.to_seq |> List.of_seq)
;;

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

let char_is_O (_, c) = c = 'O'
let tuple_of_char_and_index i c = i + 1, c (* + 1 because we want to start at 1 *)

(* solution for the part 1 *)
let part1 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file
  |> rotate_rigth
  |> List.map make_all_drop
  |> List.map (fun l ->
    l
    |> List.mapi tuple_of_char_and_index
    |> List.filter char_is_O
    |> List.map fst
    |> List.fold_left ( + ) 0)
  |> List.fold_left ( + ) 0
;;

let () = assert (part1 = 108144)

(* apply fn n times on arg *)
let n_times n fn arg =
  let rec _iter acc = function
    | i when i = n -> acc
    | i -> _iter (fn acc) (i + 1)
  in
  _iter arg 0
;;

(* apply fn 4 times on arg *)
let four_times = n_times 4

(* do a cyle of drops, aka rotate the matrix 4 times and make all drop between each
   rotation *)
let drop_cycle group =
  let string_of_char_list lst = lst |> List.to_seq |> String.of_seq in
  let transformation a =
    a |> List.map make_all_drop |> List.map string_of_char_list |> rotate_rigth
  in
  four_times transformation group
;;

(* a set where elements are a tuple of the index of the group and the group itself *)
module Set = Set.Make (struct
    type t = int * string list

    (* we only care about the group for the comparison, the index will only be
       used to know how many cycles to skip *)
    let compare (_, a) (_, b) = compare a b
  end)

let n_drop_cycle n group =
  let my_set = ref Set.empty in
  let rec _iter acc = function
    | i when i = n -> acc
    | i ->
      (match Set.find_opt (i, acc) !my_set with
       | None ->
         my_set := Set.add (i, acc) !my_set;
         _iter (drop_cycle acc) (i + 1)
       | Some (i', new_acc) ->
         (* skip all the possible cycles *)
         let diff = i - i' in
         let n = (n - i) / diff in
         let i = i + (n * diff) in
         my_set := Set.empty;
         _iter new_acc i)
  in
  _iter group 0 |> List.map (fun s -> s |> String.to_seq |> List.of_seq)
;;

(* solution for the part 2 *)
let part2 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file
  |> rotate_rigth
  |> n_drop_cycle 1000000000
  |> List.map (fun l ->
    l
    |> List.mapi tuple_of_char_and_index
    |> List.filter char_is_O
    |> List.map fst
    |> List.fold_left ( + ) 0)
  |> List.fold_left ( + ) 0
;;

let () = assert (part2 = 108404)
