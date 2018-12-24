open! Core
open! Async
open! Utils

let read_recipe_count file_name =
  let%map recipe_count = read_one_line file_name in
  Int.of_string recipe_count
;;

module Recipe_maker = struct
  type t =
    { recipes : int Doubly_linked.t
    ; recipe_count : int
    ; elf_positions : int Doubly_linked.Elt.t list sexp_opaque
    }
  [@@deriving sexp]

  let hd l = Doubly_linked.first_elt l |> Option.value_exn
  let tl l = Doubly_linked.last_elt l |> Option.value_exn

  let create () =
    let recipes = Doubly_linked.of_list [ 3; 7 ] in
    let recipe_count = 2 in
    let elf_positions = [ hd recipes; tl recipes ] in
    { recipes; recipe_count; elf_positions }
  ;;

  let recipes score =
    if score < 10
    then [ score ]
    else [ 1; score - 10 ]
  ;;

  let next_pos t elf_pos =
    let rec helper curr_pos ~step_count =
      if Int.(step_count <= 0)
      then curr_pos
      else (
        let pos =
          match Doubly_linked.next t.recipes curr_pos with
          | Some pos -> pos
          | None -> hd t.recipes
        in
        helper pos ~step_count:(step_count - 1))
    in
    let step_count = (1 + Doubly_linked.Elt.value elf_pos) mod t.recipe_count in
    helper elf_pos ~step_count
  ;;

  let tick t =
    let score =
      List.sum (module Int) t.elf_positions ~f:Doubly_linked.Elt.value
    in
    let new_recipes = recipes score in
    let recipe_count =
      List.fold new_recipes
        ~init:t.recipe_count
        ~f:(fun count recipe ->
          let (_ : int Doubly_linked.Elt.t) =
            Doubly_linked.insert_last t.recipes recipe
          in
          count + 1)
    in
    let t = { t with recipe_count } in
    let elf_positions = List.map t.elf_positions ~f:(next_pos t) in
    { t with elf_positions }
  ;;

  let rec tick_until_recipe_count t total_recipe_count =
    if t.recipe_count >= total_recipe_count
    then t
    else tick_until_recipe_count (tick t) total_recipe_count
  ;;

  let find_scores t ~first_i ~last_i =
    let rec helper elt_opt ~scores ~i =
      match elt_opt, Int.(i >= last_i) with
      | None, _ | Some _, true ->
        List.map scores ~f:Int.to_string |> String.concat
      | Some elt, false ->
        let scores =
          if Int.(i < first_i)
          then scores
          else Doubly_linked.Elt.value elt :: scores
        in
        let elt_opt = Doubly_linked.prev t.recipes elt in
        helper elt_opt ~scores ~i:(i + 1)
    in
    helper (Some (tl t.recipes)) ~scores:[] ~i:0
  ;;

  let rec tick_until_input_appears t input_str =
    let last_i_opt =
      let pattern_len = String.length input_str in
      let first_last_i_list = [ 0, pattern_len; 1, pattern_len + 1 ] in
      List.find first_last_i_list
        ~f:(fun (first_i, last_i) ->
          let pattern = find_scores t ~first_i ~last_i in
          String.(=) pattern input_str)
    in
    match last_i_opt with
    | Some (_first_i, last_i) -> t.recipe_count - last_i, t
    | None -> tick_until_input_appears (tick t) input_str
  ;;
end

let part_1 file_name =
  let%map recipe_count = read_recipe_count file_name in
  let scored_count = 10 in
  let total_recipe_count = recipe_count + scored_count in
  let recipe_maker = Recipe_maker.create () in
  let recipe_maker =
    Recipe_maker.tick_until_recipe_count recipe_maker total_recipe_count
  in
  let scores =
    let last_i = recipe_maker.recipe_count - recipe_count in
    let first_i = last_i - scored_count in
    Recipe_maker.find_scores recipe_maker ~first_i ~last_i
  in
  printf "%s\n" scores
;;

let part_2 file_name =
  let%map input = read_recipe_count file_name in
  let input_str = Int.to_string input in
  let recipe_maker = Recipe_maker.create () in
  let recipe_count_before_input, (_recipe_maker : Recipe_maker.t) =
    Recipe_maker.tick_until_input_appears recipe_maker input_str
  in
  printf "%d\n" recipe_count_before_input
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
