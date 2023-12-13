let input_test = "./inputs/day9/test.txt"
let input_file = "./inputs/day9/input.txt"

(* next element *)
let next_in_sec top bottom = top - bottom

let diffs lst =
  let do_fold acc curr =
    match acc with
    | [] -> [ curr ]
    | hd :: tl -> curr :: (curr - hd) :: tl
  in
  List.fold_left do_fold [] lst |> List.tl |> List.rev
;;

(* check if all elements of a list are 0 *)
let is_all_zeros lst = List.for_all (fun x -> x = 0) lst

(* till we get a list full of zeros, turn the last list into a new one with the
   diff of each pair *)
let diffs_till_zeros lst =
  let rec _internal acc lst =
    let d = diffs lst in
    let acc = d :: acc in
    if is_all_zeros d then acc else _internal acc d
  in
  _internal [ lst ] lst
;;

(* get the last element of a list *)
let last lst = lst |> List.rev |> List.hd

let find_next lst =
  lst
  |> diffs_till_zeros
  |> List.map last
  |> List.fold_left (fun acc curr -> curr + acc) 0
;;

let find_prev lst =
  lst
  |> diffs_till_zeros
  |> List.map List.hd
  |> List.fold_left (fun acc curr -> curr - acc) 0
;;

let common file fn =
  Utils.read_input file
  |> List.map Utils.parse_line_of_nums
  |> List.map fn
  |> List.fold_left (fun acc curr -> curr + acc) 0
;;

(* solutions *)
let part1 = common input_file find_next
let () = assert (part1 = 1637452029)

(* solution of the part 2*)
let part2 = common input_file find_prev
let () = assert (part2 = 908)
