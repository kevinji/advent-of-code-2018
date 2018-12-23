open! Core
open! Async
open! Utils

let alive_state_to_char = function
  | true -> "#"
  | false -> "."
;;

let alive =
  let t_of_char = function
    | "#" -> true
    | "." -> false
    | s -> raise_s [%message "Invalid plant state found" (s : string)]
  in
  Tyre.conv t_of_char alive_state_to_char [%tyre "[#\\.]"]
;;

let alive_state =
  let t_of_seq (alive, alive_seq) =
    Seq.fold_left (fun l ele -> ele :: l) [ alive ] alive_seq
    |> List.rev
  in
  let seq_of_t l =
    let hd, l =
      match l with
      | hd :: tl -> hd, tl
      | [] -> raise_s [%message "Plant state list is empty"]
    in
    let seq =
      List.fold_right l
        ~init:(fun () -> Seq.Nil)
        ~f:(fun ele seq () -> Seq.Cons (ele, seq))
    in
    hd, seq
  in
  Tyre.conv t_of_seq seq_of_t [%tyre "(?&alive)+"]
;;

let initial_state_re =
  let open Tyre in
  [%tyre "initial state: (?&alive_state)"] |> compile
;;

module Rule = struct
  type t =
    { state : bool list
    ; result : bool
    }
  [@@deriving sexp]

  let re =
    let open Tyre in
    [%tyre "(?&state:alive_state) => (?&result:alive)"] |> compile
  ;;

  let of_object obj = { state = obj#state; result = obj#result }

  let length = 5
end

module Rules = struct
  (** A mutable trie that consolidates a [Rule.t list]. *)
  type t =
    { mutable value : bool
    ; mutable true_ : t option
    ; mutable false_ : t option
    }
  [@@deriving sexp]

  let empty () = { value = false; true_ = None; false_ = None }

  let process_rule t (rule : Rule.t) =
    if rule.result
    then (
      let node =
        List.fold rule.state
          ~init:t
          ~f:(fun node is_alive ->
            if is_alive
            then (
              match node.true_ with
              | Some node -> node
              | None ->
                let new_node = empty () in
                node.true_ <- Some new_node;
                new_node)
            else (
              match node.false_ with
              | Some node -> node
              | None ->
                let new_node = empty () in
                node.false_ <- Some new_node;
                new_node))
      in
      node.value <- true)
  ;;

  let of_list l =
    let t = empty () in
    List.iter l ~f:(process_rule t);
    t
  ;;

  let next_node t = function
    | true -> t.true_
    | false -> t.false_
  ;;
end

module State = struct
  type t =
    { plants : bool array
    ; mutable left_i : int (** Index (<= 0) of the left-most plant. *)
    ; mutable right_i : int (** Index + 1 of the right-most plant. *)
    ; rules : Rules.t sexp_opaque
    }
  [@@deriving sexp]

  let of_list l ~rules =
    { plants = Array.of_list l
    ; left_i = 0
    ; right_i = List.length l
    ; rules
    }
  ;;

  let size t = t.right_i - t.left_i

  let copy_with_same_size t =
    { t with plants = Array.create ~len:(Array.length t.plants) false }
  ;;

  let should_expand t new_size = Int.(>) new_size (Array.length t.plants)

  let expand t =
    let curr_len = Array.length t.plants in
    let new_len = 3 * curr_len / 2 in
    let first_neg_i =
      if t.left_i >= 0 then curr_len else Array.normalize t.plants t.left_i
    in
    let new_first_neg_i =
      if t.left_i >= 0 then new_len else new_len + t.left_i
    in
    let new_plants =
      Array.init new_len
        ~f:(fun i ->
          if i < first_neg_i
          then t.plants.(i)
          else if i < new_first_neg_i
          then false
          else t.plants.(i - (new_len - curr_len)))
    in
    { t with plants = new_plants }
  ;;

  let set_alive t i =
    let t =
      let size_increase =
        if i < t.left_i then t.left_i - i
        else if i >= t.right_i then i - t.right_i + 1
        else 0
      in
      if should_expand t Int.(size t + size_increase)
      then expand t
      else t
    in
    Array.nset t.plants i true;
    t.left_i <- Int.min t.left_i i;
    t.right_i <- Int.max t.right_i (i + 1);
    t
  ;;

  let is_new_plant_alive t ~left_i =
    let rec helper rules ~left_i ~pots_remaining =
      let is_alive =
        if left_i < t.left_i || left_i >= t.right_i
        then false
        else Array.nget t.plants left_i
      in
      match Rules.next_node rules is_alive with
      | None -> false
      | Some new_rules ->
        if Int.(pots_remaining <= 1)
        then new_rules.value
        else (
          helper new_rules
            ~left_i:(left_i + 1)
            ~pots_remaining:(pots_remaining - 1))
    in
    helper t.rules ~left_i ~pots_remaining:Rule.length
  ;;

  let step t =
    let new_t = copy_with_same_size t in
    let half_rule_len = Rule.length / 2 in
    let leftmost_i = t.left_i - half_rule_len in
    let rightmost_i = t.right_i + half_rule_len in
    let rec helper new_t ~curr_i =
      if curr_i >= rightmost_i
      then new_t
      else (
        let is_alive =
          is_new_plant_alive t ~left_i:(curr_i - half_rule_len)
        in
        let new_t =
          if is_alive
          then set_alive new_t curr_i
          else new_t
        in
        helper new_t ~curr_i:(curr_i + 1))
    in
    helper new_t ~curr_i:leftmost_i
  ;;

  let to_string t =
    let plant_chars =
      Array.init (size t)
        ~f:(fun i -> Array.nget t.plants (t.left_i + i) |> alive_state_to_char)
    in
    String.concat_array plant_chars
  ;;

  let sum_of_alive_plant_indices t =
    Sequence.range t.left_i t.right_i
    |> Sequence.filter ~f:(Array.nget t.plants)
    |> Sequence.sum (module Int) ~f:Fn.id
  ;;
end

let read_initial_state_and_rules file_name =
  let%map initial_state, rules =
    match%map Reader.file_lines file_name with
    | initial_state :: "" :: rules -> initial_state, rules
    | _ -> raise_s [%message "Invalid number of lines found"]
  in
  let rules =
    List.map rules ~f:(fun rule -> exec_re Rule.re rule |> Rule.of_object)
    |> Rules.of_list
  in
  let initial_state =
    exec_re initial_state_re initial_state
    |> State.of_list ~rules
  in
  initial_state
;;

let rec step_state ?(debug = false) state ~n =
  if debug then printf !"%{State}\n" state;
  if Int.(n <= 0)
  then state
  else step_state (State.step state) ~n:(n - 1)
;;

let part_1 file_name =
  let%map initial_state = read_initial_state_and_rules file_name in
  let new_state = step_state initial_state ~n:20 in
  printf "%d\n" (State.sum_of_alive_plant_indices new_state)
;;

let part_2 file_name =
  let%map initial_state = read_initial_state_and_rules file_name in
  (* Building up from smaller cases:
        500: 94023
       5000: 931023
      50000: 9301023
     500000: 93001023

     Following this pattern, 50_000_000_000 (10 zeros) should map to
     93_[7 zeros]_1023.
  *)
  let new_state = step_state initial_state ~n:50_000 in
  printf "%d\n" (State.sum_of_alive_plant_indices new_state)
;;

let cmd_part_1 =
  Command.async
    ~summary:"Part 1"
    (let open Command.Let_syntax in
     let%map_open input_file =
       flag "-input" (required file) ~doc:"FILE path to input file"
     in
     fun () -> part_1 input_file)
;;

let cmd_part_2 =
  Command.async
    ~summary:"Part 2"
    (let open Command.Let_syntax in
     let%map_open input_file =
       flag "-input" (required file) ~doc:"FILE path to input file"
     in
     fun () -> part_2 input_file)
;;

let command =
  Command.group
    ~summary:"Day 12"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
