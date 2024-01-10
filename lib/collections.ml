module Heap : sig
  type 'a t

  exception Empty

  val create : ?cmp:('a -> 'a -> int) -> unit -> 'a t
  val push : 'a -> 'a t -> unit
  val take : 'a t -> 'a
  val take_opt : 'a t -> 'a option
  val peek : 'a t -> 'a
  val peek_opt : 'a t -> 'a option
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val clear : 'a t -> unit
end = struct
  type 'a t =
    { mutable data : 'a array (* the elements in the queue *)
    ; mutable hd : int (* idx of the first element *)
    ; mutable tl : int (* idx of the last element *)
    ; cmp : 'a -> 'a -> int (* compare function *)
    }

  exception Empty

  let make n = Array.make n (Obj.magic 0)

  (* given a compare function, create a new queue.
     The array will start with 10 elements *)
  let create ?(cmp = compare) () = { data = make 10; hd = 0; tl = 0; cmp }

  (* if idxs meet, then the queue is empty *)
  let is_empty q = q.hd = q.tl

  (* the actual size of the queue *)
  let length { hd; tl; _ } = tl - hd

  (* utility fns to navigate to/from the parent *)
  let parent_idx idx = (idx - 1) / 2
  let left_idx idx = (2 * idx) + 1
  let right_idx idx = left_idx idx + 1

  let swap { data; _ } i j =
    let tmp = data.(i) in
    data.(i) <- data.(j);
    data.(j) <- tmp
  ;;

  (* grow the capacity of the queue *)
  let grow q =
    (* create a new array with double the size of the current one *)
    let l = length q in
    let new_arr = make (l * 2) in
    (* copy the elements from the old array to the new one *)
    Array.blit q.data q.hd new_arr 0 l;
    (* update the queue *)
    q.data <- new_arr;
    (* update the head and tail *)
    q.hd <- 0;
    q.tl <- l
  ;;

  (* true if the element at i is less than the one at j, according to the
     compare function *)
  let is_less q i j = q.cmp q.data.(i) q.data.(j) < 0

  (* add a new element to the queue *)
  let push v q =
    if q.tl = Array.length q.data then grow q;

    q.data.(q.tl) <- v;
    q.tl <- q.tl + 1;

    (* bubble up the last element *)
    let rec bubble_up idx =
      if idx = q.hd
      then ()
      else (
        let parent = parent_idx idx in
        if is_less q idx parent
        then (
          swap q idx parent;
          bubble_up parent))
    in

    bubble_up (q.tl - 1)
  ;;

  (* take the first element from the queue *)
  let take q =
    if is_empty q
    then raise Empty
    else (
      (* store the value of the first element *)
      let v = q.data.(q.hd) in

      (* move the last element to the front *)
      q.data.(q.hd) <- q.data.(q.tl - 1);
      q.data.(q.tl - 1) <- Obj.magic 0;
      q.tl <- q.tl - 1;

      (* bubble down the hd element *)
      let rec bubble_down idx =
        (* get the idx of the children *)
        let left = left_idx idx in
        let right = right_idx idx in

        (* set idx as the idx of the smallest value, for now *)
        let smallest = ref idx in

        (* determine the new smallest's idx *)
        if left < q.tl && is_less q left !smallest then smallest := left;
        if right < q.tl && is_less q right !smallest then smallest := right;

        (* if the smallest is not the current idx, then swap the values
           and bubble down the new idx *)
        if !smallest <> idx
        then (
          swap q idx !smallest;
          bubble_down !smallest)
      in
      bubble_down q.hd;

      (* return the value *)
      v)
  ;;

  let take_opt q =
    try Some (take q) with
    | Empty -> None
  ;;

  let peek q = if is_empty q then raise Empty else q.data.(q.hd)

  let peek_opt q =
    try Some (peek q) with
    | Empty -> None
  ;;

  (* clear the queue *)
  let clear q =
    q.hd <- 0;
    q.tl <- 0;
    q.data <- make 10
  ;;
end

let () =
  let q = Heap.create ~cmp:Int.compare () in

  let input = [ 5; 4; 3; 2; 1; 5; 4; 3; 2; 1; 5; 4; 3; 2; 1 ] in
  let expected = List.sort compare input in

  input |> List.iter (fun n -> Heap.push n q);
  let res = List.init (List.length input) (fun _ -> Heap.take q) in

  assert (res = expected)
;;
