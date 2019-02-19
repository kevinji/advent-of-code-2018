open! Core
open! Async
open! Utils

module Creature_type = struct
  type t =
    | Goblin
    | Elf
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]
end

module Move_state = struct
  type t =
    | A
    | B
  [@@deriving sexp, compare]

  let equal = [%compare.equal: t]

  let other = function
    | A -> B
    | B -> A
  ;;
end

module Creature = struct
  type t =
    { type_ : Creature_type.t
    ; hp : int
    ; attack : int
    ; move_state : Move_state.t
    }
  [@@deriving sexp]

  let goblin () = { type_ = Goblin; hp = 200; attack = 3; move_state = A }
  let elf ~attack = { type_ = Elf; hp = 200; attack; move_state = A }

  module Attack_result = struct
    type nonrec t =
      | Alive of t
      | Dead
    [@@deriving sexp]
  end

  let should_attack t other = not (Creature_type.equal t.type_ other.type_)

  let attack t other : Attack_result.t =
    let other_hp = other.hp - t.attack in
    if Int.is_positive other_hp
    then Alive { other with hp = other_hp }
    else Dead
  ;;

  let has_moved t ~unmoved_state =
    not (Move_state.equal t.move_state unmoved_state)
  ;;

  let set_moved t = { t with move_state = Move_state.other t.move_state }
end

module Square = struct
  type t =
    | Empty
    | Wall
    | Occupied of Creature.t
  [@@deriving sexp]

  let of_char ~elf_attack = function
    | '.' -> Empty
    | '#' -> Wall
    | 'G' -> Occupied (Creature.goblin ())
    | 'E' -> Occupied (Creature.elf ~attack:elf_attack)
    | _ as ch -> raise_s [%message "Unknown board character found" (ch : char)]
  ;;

  let to_char = function
    | Empty -> '.'
    | Wall -> '#'
    | Occupied creature ->
      (match creature.type_ with
       | Goblin -> 'G'
       | Elf -> 'E')
  ;;
end

module Point = struct
  module T = struct
    type t = int * int [@@deriving sexp]

    let compare (x1, y1) (x2, y2) =
      let cmp = Int.compare y1 y2 in
      if Int.(cmp <> 0)
      then cmp
      else Int.compare x1 x2
    ;;
  end

  include T
  include Comparable.Make (T)
end

module Game = struct
  type t =
    { board : Square.t array array
    ; goblin_count : int
    ; elf_count : int
    ; unmoved_state : Move_state.t
    }
  [@@deriving sexp]

  let of_lines ?(elf_attack = 3) lines =
    let game =
      let board =
        Array.make_matrix
          ~dimx:(List.hd_exn lines |> String.length)
          ~dimy:(List.length lines)
          Square.Empty
      in
      { board; goblin_count = 0; elf_count = 0; unmoved_state = A }
    in
    List.foldi lines ~init:game ~f:(fun y game ->
      String.foldi ~init:game ~f:(fun x game square_ch ->
        let square = Square.of_char square_ch ~elf_attack in
        let goblin_count, elf_count =
          match square with
          | Occupied creature ->
            (match creature.type_ with
             | Goblin -> game.goblin_count + 1, game.elf_count
             | Elf -> game.goblin_count, game.elf_count + 1)
          | _ -> game.goblin_count, game.elf_count
        in
        game.board.(x).(y) <- square;
        { game with goblin_count; elf_count }))
  ;;

  let other_count t creature_type =
    match (creature_type : Creature_type.t) with
    | Goblin -> t.elf_count
    | Elf -> t.goblin_count
  ;;

  let get_square t (x, y) = t.board.(x).(y)

  let set_square t (x, y) square = t.board.(x).(y) <- square

  let print_board t =
    Array.transpose_exn t.board
    |> Array.iter ~f:(fun row ->
      Array.iter row ~f:(Fn.compose print_char Square.to_char);
      print_newline ())
  ;;

  let is_in_bounds t (x, y) =
    let max_x = Array.length t.board in
    let max_y = Array.length t.board.(0) in
    Int.(0 <= x) && Int.(x < max_x) && Int.(0 <= y) && Int.(y < max_y)
  ;;

  let get_neighbors t (x, y) =
    (* Squares in reading order. *)
    [ 0, -1; -1, 0; 1, 0; 0, 1 ]
    |> List.filter_map ~f:(fun (dx, dy) ->
      let neighbor = x + dx, y + dy in
      Option.some_if (is_in_bounds t neighbor) neighbor)
  ;;

  module Pos_search_element = struct
    type t =
      { depth : int
      ; prev_pos : Point.t
      ; new_pos : Point.t [@compare.ignore]
      ; first_pos : Point.t
      }
    [@@deriving sexp, compare]
  end

  (* Best next position is closest *adjacent square to target* by reading order,
     with ties for next position also broken by reading order. *)
  let find_next_pos_to_closest_target t (creature : Creature.t) pos =
    (* Unfortunately, using a queue is not sufficient because we will not visit
       positions in the correct order. We use a heap instead to maintain the
       invariant that positions are visited in reading order. *)
    let rec helper ~visited ~heap =
      let open Option.Let_syntax in
      let%bind { Pos_search_element.depth; new_pos; first_pos; _ } =
        Heap.pop heap
      in
      if Set.mem visited new_pos
      then helper ~visited ~heap
      else (
        let visited = Set.add visited new_pos in
        match get_square t new_pos with
        | Wall -> helper ~visited ~heap
        | Empty ->
          get_neighbors t new_pos
          |> List.map ~f:(fun neighbor_pos ->
              { Pos_search_element.
                depth = depth + 1
              ; prev_pos = new_pos
              ; new_pos = neighbor_pos
              ; first_pos
              })
          |> List.iter ~f:(Heap.add heap);
          helper ~visited ~heap
        | Occupied other_creature ->
          if Creature.should_attack creature other_creature
          then Option.some_if (not (Point.equal first_pos new_pos)) first_pos
          else helper ~visited ~heap)
    in
    let visited = Point.Set.singleton pos in
    let heap =
      get_neighbors t pos
      |> List.map ~f:(fun neighbor_pos ->
          { Pos_search_element.
            depth = 1
          ; prev_pos = pos
          ; new_pos = neighbor_pos
          ; first_pos = neighbor_pos
          })
      |> Heap.of_list ~cmp:Pos_search_element.compare
    in
    helper ~visited ~heap
  ;;

  (* Best target has lowest HP, with ties broken by reading order. *)
  let find_best_target t (creature : Creature.t) pos =
    get_neighbors t pos
    |> List.filter_map ~f:(fun new_pos ->
      match get_square t new_pos with
      | Empty | Wall -> None
      | Occupied other_creature ->
        if Creature.should_attack creature other_creature
        then Some (new_pos, other_creature)
        else None)
    |> List.min_elt ~compare:(fun (pos1, creature1) (pos2, creature2) ->
      let cmp = Int.compare creature1.hp creature2.hp in
      if Int.(cmp <> 0)
      then cmp
      else Point.compare pos1 pos2)
  ;;

  let take_turn t (creature : Creature.t) pos =
    let creature = Creature.set_moved creature in
    (* Creatures should try moving first before attacking. *)
    let pos =
      match find_next_pos_to_closest_target t creature pos with
      | None -> pos
      | Some next_pos ->
        set_square t pos Empty;
        next_pos
    in
    set_square t pos (Occupied creature);
    match find_best_target t creature pos with
    | None -> t
    | Some (other_pos, other_creature) ->
      (match Creature.attack creature other_creature with
       | Alive other_creature ->
         set_square t other_pos (Occupied other_creature);
         t
       | Dead ->
         set_square t other_pos Empty;
         match other_creature.type_ with
         | Goblin -> { t with goblin_count = t.goblin_count - 1 }
         | Elf -> { t with elf_count = t.elf_count - 1 })
  ;;

  module Round_result = struct
    type nonrec t =
      | End_of_round of t
      | End_of_combat of t
    [@@deriving sexp]
  end

  let run_round t =
    let max_x = Array.length t.board in
    let max_y = Array.length t.board.(0) in
    Sequence.cartesian_product
      (Sequence.range 0 max_y)
      (Sequence.range 0 max_x)
    |> Sequence.fold_until
      ~init:t
      ~f:(fun t (y, x) ->
        let pos = x, y in
        match get_square t pos with
        | Empty | Wall -> Continue t
        | Occupied creature
          when Creature.has_moved creature ~unmoved_state:t.unmoved_state ->
          (* Creatures should not move twice. *)
          Continue t
        | Occupied creature ->
          let other_count = other_count t creature.type_ in
          if Int.is_non_positive other_count
          then Stop (Round_result.End_of_combat t)
          else Continue (take_turn t creature pos))
      ~finish:(fun t ->
        let t = { t with unmoved_state = Move_state.other t.unmoved_state } in
        Round_result.End_of_round t)
  ;;

  let outcome t ~round_count =
    printf "Round count: %d\n" round_count;
    let hp_sum =
      Array.sum (module Int) t.board ~f:(fun col ->
        Array.sum (module Int) col ~f:(function
          | Empty | Wall -> 0
          | Occupied creature -> creature.hp))
    in
    printf "HP sum: %d\n" hp_sum;
    hp_sum * round_count
  ;;

  let run_to_completion t =
    let rec helper t ~round_count =
      match run_round t with
      | End_of_round t ->
        helper t ~round_count:(round_count + 1)
      | End_of_combat t ->
        let outcome = outcome t ~round_count in
        t, outcome
    in
    helper t ~round_count:0
  ;;

  let run_to_completion_if_no_elves_die t =
    let rec helper old_t ~round_count =
      let open Option.Let_syntax in
      match run_round old_t with
      | End_of_round t ->
        let%bind () = Option.some_if (Int.(old_t.elf_count = t.elf_count)) () in
        helper t ~round_count:(round_count + 1)
      | End_of_combat t ->
        let%map () = Option.some_if (Int.(old_t.elf_count = t.elf_count)) () in
        let outcome = outcome t ~round_count in
        t, outcome
    in
    helper t ~round_count:0
  ;;
end

let part_1 file_name =
  let%map game =
    let%map game_lines = Reader.file_lines file_name in
    Game.of_lines game_lines
  in
  Game.print_board game;
  print_newline ();
  let game, outcome = Game.run_to_completion game in
  printf "Outcome: %d\n" outcome;
  print_newline ();
  Game.print_board game
;;

let part_2 file_name =
  let%map game_lines = Reader.file_lines file_name in
  let elf_attack_seq =
    Sequence.unfold ~init:4 ~f:(fun attack -> Some (attack, attack + 1))
  in
  let game, outcome =
    Sequence.find_map elf_attack_seq ~f:(fun elf_attack ->
      let open Option.Let_syntax in
      let game = Game.of_lines game_lines ~elf_attack in
      let%map game, outcome = Game.run_to_completion_if_no_elves_die game in
      printf "Elf attack: %d\n" elf_attack;
      game, outcome)
    |> Option.value_exn
      ~here:[%here]
      ~message:"Unexpected exception: elf attack sequence should never end"
  in
  printf "Outcome: %d\n" outcome;
  print_newline ();
  Game.print_board game
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
    ~summary:"Day 14"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
