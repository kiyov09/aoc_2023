(** whether a char is a digit or not *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(** turn a digit char into an int *)
let int_of_digit_char c = int_of_string_opt (Char.escaped c)

(** Read from a channel till there's no more to read *)
let read_all ch =
  let rec aux acc ch =
    match In_channel.input_line ch with
    | None -> List.rev acc
    | Some s -> aux (s :: acc) ch
  in
  aux [] ch
;;

(** turn a string into a list of chars *)
let char_list_of_string s = s |> String.to_seq |> List.of_seq

(** whitespace split a string *)
let split_ws s = s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")

(** Read all lines from a file given its path *)
let read_input input_file = open_in input_file |> read_all

(** skip n items in a list *)
let rec skip n = function
  | [] -> []
  | _ :: xs when n > 0 -> skip (n - 1) xs
  | xs -> xs
;;

(* turn a string consisting of space separated numbers into a list of ints *)
let parse_line_of_nums line = line |> split_ws |> List.map int_of_string

(* sum a list of ints *)
let sum = List.fold_left ( + ) 0
