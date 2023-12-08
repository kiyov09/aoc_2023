let input_test = "./inputs/day7/test.txt"
let input_test_2 = "./inputs/day7/test2.txt"
let input_file = "./inputs/day7/input.txt"

(* One of the possible cards *)
type card =
  | Number of int
  | Ten
  | Jack
  | Queen
  | King
  | Ace

(* pretty name for a list of cards *)
type hand = card list

(* type of a hand based on the cards on it *)
type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

(* a play is a hand and a bid (these are each line of the input ) *)
type play =
  { hand : hand
  ; bid : int
  }

(* get a card from a char *)
let card_of_char = function
  | '2' .. '9' as c -> Number (int_of_char c - int_of_char '0')
  | 'T' -> Ten
  | 'J' -> Jack (* or Joker on the part 2 *)
  | 'Q' -> Queen
  | 'K' -> King
  | 'A' -> Ace
  | _ -> failwith "invalid card"
;;

(* compare cards based on the type FiveOfAKind being the highest and HighCard the
   lowest. If both cards are of the same type, compare the values of each card *)
let compare_cards is_j_joker c1 c2 =
  let int_of_card = function
    | Number n -> n
    | Ten -> 10
    | Jack -> if is_j_joker then 1 else 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14
  in
  compare (int_of_card c1) (int_of_card c2)
;;

(* get a hand type from a hand of cards. See puzzle description for the rule *)
let hand_type_of_hand hand =
  let cmp_c = compare_cards false in
  let gouped =
    hand
    |> List.sort cmp_c
    |> List.to_seq
    (* group cards by type *)
    |> Seq.group (fun c1 c2 -> cmp_c c1 c2 = 0)
    (* make everything a list of lists *)
    |> List.of_seq
    |> List.map List.of_seq
    (* Sort it by the length of each list in descending order *)
    |> List.sort (fun g1 g2 -> compare (List.length g2) (List.length g1))
  in
  match gouped with
  (* a group of 5 *)
  | [ [ _; _; _; _; _ ] ] -> FiveOfAKind
  (* a group of 4 and a group of 1 *)
  | [ [ _; _; _; _ ]; [ _ ] ] -> FourOfAKind
  (* a group of 3 and a group of 2 *)
  | [ [ _; _; _ ]; [ _; _ ] ] -> FullHouse
  (* a group of 3 and 2 groups of 1 *)
  | [ [ _; _; _ ]; [ _ ]; [ _ ] ] -> ThreeOfAKind
  (* 2 groups of 2 *)
  | [ [ _; _ ]; [ _; _ ]; [ _ ] ] -> TwoPair
  (* 1 group of 2 and 3 groups of 1 *)
  | [ [ _; _ ]; [ _ ]; [ _ ]; [ _ ] ] -> OnePair
  (* 5 groups of 1 *)
  | _ -> HighCard
;;

(* get a hand type from a hand of cards. This time J are Jokers so we need to
   "improve" the type of the hand *)
let hand_type_of_hand_with_jokers hand =
  let hand_type = hand_type_of_hand hand in
  let js =
    List.filter
      (function
       | Jack -> true
       | _ -> false)
      hand
    |> List.length
  in
  match js, hand_type with
  | _, FiveOfAKind -> FiveOfAKind
  | 4, FourOfAKind -> FiveOfAKind
  | 3, FullHouse -> FiveOfAKind
  | 3, ThreeOfAKind -> FourOfAKind
  | 2, FullHouse -> FiveOfAKind
  | 2, TwoPair -> FourOfAKind
  | 2, OnePair -> ThreeOfAKind
  | 1, FourOfAKind -> FiveOfAKind
  | 1, ThreeOfAKind -> FourOfAKind
  | 1, TwoPair -> FullHouse
  | 1, OnePair -> ThreeOfAKind
  | 1, HighCard -> OnePair
  | 0, _ -> hand_type
  | _, _ -> failwith "unreachable!"
;;

(* compare hand types based on the type FiveOfAKind being the highest and
   HighCard the lowest *)
let compare_hand_types h1 h2 =
  let int_of_hand_type = function
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
  in
  compare (int_of_hand_type h1) (int_of_hand_type h2)
;;

(* compare two hands based on their types using cmp_c as the fn to compare
   individual cards and ht_of_h as the fn to get a hand_type from a hand *)
let compare_hands cmp_c ht_of_h h1 h2 =
  let rec compare_same h1 h2 =
    match h1, h2 with
    | [], [] -> 0
    | [], _ | _, [] -> failwith "unreachable!"
    | c1 :: t1, c2 :: t2 ->
      let cmp = cmp_c c1 c2 in
      if cmp = 0 then compare_same t1 t2 else cmp
  in
  match compare_hand_types (ht_of_h h1) (ht_of_h h2) with
  | 0 -> compare_same h1 h2
  | n -> n
;;

(* compare two plays based on their hands, applying fn *)
let compare_plays fn { hand = h1; _ } { hand = h2; _ } = fn h1 h2

(* --- parsing --- *)

let parse_line line =
  match String.split_on_char ' ' line with
  | [ hand; bid ] ->
    let bid = int_of_string bid in
    let hand = hand |> String.to_seq |> List.of_seq |> List.map card_of_char in
    { hand; bid }
  | _ -> failwith "invalid line"
;;

(* --- end parsing --- *)

let common file cmp =
  Utils.read_input file
  |> List.map parse_line
  |> List.sort cmp
  |> List.mapi (fun i p -> i + 1, p)
  |> List.fold_left (fun acc (i, p) -> acc + (p.bid * i)) 0
;;

let part1 =
  let cmp_c = compare_cards false in
  let cmp = compare_hands cmp_c hand_type_of_hand in
  (* common input_test (compare_plays cmp) *)
  common input_file (compare_plays cmp)
;;

let () = assert (part1 = 253638586)

let part2 =
  let cmp_c = compare_cards true in
  let cmp = compare_hands cmp_c hand_type_of_hand_with_jokers in
  (* common input_test_2 (compare_plays cmp) *)
  common input_file (compare_plays cmp)
;;

let () = assert (part2 = 253253225)
