let input_test = "./inputs/day1/test.txt"
let input_test_2 = "./inputs/day1/test2.txt"
let input_file = "./inputs/day1/input.txt"

type digits =
  { first : int
  ; last : int
  }

(* given a digits, return the correspondint int *)
let int_of_digits d =
  let { first; last } = d in
  (first * 10) + last
;;

(** accumulate into a digits the digits of a line *)
let fold_fn acc c =
  match acc with
  | None -> Some { first = c; last = c }
  | Some acc -> Some { acc with last = c }
;;

(** given a line (string), process each line in the following way:
    - convert the string into a list of chars
    - filter map the list of chars to a list of int
    - fold the list of int into a digits *)
let digits_of_string s =
  s
  |> Utils.char_list_of_string
  |> List.filter_map Utils.int_of_digit_char
  |> List.fold_left fold_fn None
;;

(** given a list of chars, return a tuple containing an optional int and the rest
    of the list *)
let rec get_next_int = function
  | 'o' :: ('n' :: 'e' :: _ as lt) -> Some 1, lt
  | 't' :: ('w' :: 'o' :: _ as lt) -> Some 2, lt
  | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as lt) -> Some 3, lt
  | 'f' :: ('o' :: 'u' :: 'r' :: _ as lt) -> Some 4, lt
  | 'f' :: ('i' :: 'v' :: 'e' :: _ as lt) -> Some 5, lt
  | 's' :: ('i' :: 'x' :: _ as lt) -> Some 6, lt
  | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as lt) -> Some 7, lt
  | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as lt) -> Some 8, lt
  | 'n' :: ('i' :: 'n' :: 'e' :: _ as lt) -> Some 9, lt
  | hd :: lt when Utils.is_digit hd -> Utils.int_of_digit_char hd, lt
  | _ :: lt -> get_next_int lt
  | _ -> None, []
;;

(** given a list of chars, return a tuple option containing the next int and the
    rest of the list *)
let next_int lst =
  match get_next_int lst with
  | Some i, lt -> Some (i, lt)
  | None, _ -> None
;;

(** given a line (string), process each line, this time taking into account that
    some digits may be written in words *)
let digits_of_string' s =
  s |> Utils.char_list_of_string |> Seq.unfold next_int |> Seq.fold_left fold_fn None
;;

(* common function to solve both parts *)
let common file fn =
  file
  |> Utils.read_input
  |> List.filter_map fn
  |> List.map int_of_digits
  |> List.fold_left ( + ) 0
;;

(* solution of the part 1*)
let part1 = common input_file digits_of_string

(* solution of the part 2*)
let part2 = common input_file digits_of_string'
