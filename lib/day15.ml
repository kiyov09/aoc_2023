let input_test = "./inputs/day15/test.txt"
let input_file = "./inputs/day15/input.txt"

(* for readability *)
let mod_256 a = a mod 256
let times_17 a = a * 17

(* hash a char
     - char code
     - + acc
     - * 17
     - mod 256
*)
let hash_char acc c = c |> Char.code |> ( + ) acc |> times_17 |> mod_256

(* hash a string, fold the string applying hash_char on every char *)
let hash_string s = s |> String.fold_left hash_char 0

(* solution for the part1 *)
let part1 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file
  |> List.map (String.split_on_char ',')
  |> List.flatten
  |> List.map hash_string
  |> Utils.sum
;;

let () = assert (part1 = 511215)

type op =
  | Add
  | Remove

type lens =
  { label : string
  ; op : op
  ; focal_length : int
  }

let lens_of_string s =
  if String.ends_with ~suffix:"-" s
  then { label = s |> String.split_on_char '-' |> List.hd; op = Remove; focal_length = 1 }
  else (
    let split = s |> String.split_on_char '=' in
    { label = List.hd split; op = Add; focal_length = List.nth split 1 |> int_of_string })
;;

let hash_of_lens l = hash_string l.label

(* all boxes are empty *)
let boxes = ref (Array.make 256 ([] : lens list))

(* update boxes based on the lens *)
let update_boxes l =
  let hash = hash_of_lens l in
  match l.op with
  (* remove the "lens" from the corresponding box *)
  | Remove -> !boxes.(hash) <- !boxes.(hash) |> List.filter (fun x -> x.label <> l.label)
  | Add ->
    (* get the box cux we'll act on it from more than one branch *)
    let box = !boxes.(hash) in
    (match box with
     (* if no lens in the box, add the lens *)
     | [] -> !boxes.(hash) <- [ l ]
     | box ->
       (* compare two lenses based on their label *)
       let equal l1 l2 = l1.label = l2.label in
       (* if the lens is already in the box, update it, else add it to the box *)
       if List.exists (equal l) box
       then !boxes.(hash) <- box |> List.map (fun x -> if equal x l then l else x)
       else !boxes.(hash) <- box @ [ l ])
;;

let calculate_focusing_power boxes =
  boxes
  |> Array.mapi (fun i box ->
    box |> List.mapi (fun j l -> (1 + i) * (1 + j) * l.focal_length))
  |> Array.to_list
  |> List.flatten
  |> Utils.sum
;;

(* solution for the part2 *)
let part2 =
  (* Utils.read_input input_test *)
  Utils.read_input input_file
  |> List.map (String.split_on_char ',')
  |> List.flatten
  |> List.map lens_of_string
  |> List.iter update_boxes;
  calculate_focusing_power !boxes
;;

let () = assert (part2 = 236057)
