open! Core
open! Async

module G = Graph.Imperative.Digraph.Concrete (Char)

let edge_re =
  Re.Perl.compile_pat "Step ([A-Z]) must be finished before \
                       step ([A-Z]) can begin."
;;

let edge_of_string string =
  let groups = Re.exec edge_re string in
  let from = Re.Group.get groups 1 |> Char.of_string in
  let to_ = Re.Group.get groups 2 |> Char.of_string in
  G.E.create from () to_
;;

let read_dag file_name =
  let%map edge_strings = Reader.file_lines file_name in
  let edges = List.map edge_strings ~f:edge_of_string in
  let dag = G.create () in
  List.iter edges ~f:(G.add_edge_e dag);
  dag
;;

let sources dag =
  G.fold_vertex
    (fun v sources ->
       if Int.(G.in_degree dag v = 0)
       then Set.add sources v
       else sources)
    dag
    Char.Set.empty
;;

let new_sources dag source =
  let dests = G.succ dag source in
  List.filter dests
    ~f:(fun dest ->
      G.remove_edge dag source dest;
      Int.(G.in_degree dag dest = 0))
  |> Char.Set.of_list
;;

let _topological_sort dag =
  let rec helper ~order_rev ~sources =
    if Set.is_empty sources
    then List.rev order_rev
    else (
      (* Rely on the fact that [sources] is ordered. *)
      let source = Set.min_elt_exn sources in
      let order_rev = source :: order_rev in
      let sources =
        Set.remove sources source
        |> Set.union (new_sources dag source)
      in
      helper ~order_rev ~sources)
  in
  helper ~order_rev:[] ~sources:(sources dag)
;;

module Topological = Graph.Topological.Make_stable (G)

let topological_sort dag =
  Topological.fold List.cons dag [] |> List.rev
;;

let part_1 file_name =
  let%map dag = read_dag file_name in
  let order = topological_sort dag |> String.of_char_list in
  print_endline order
;;

module Worker = struct
  module T = struct
    type t =
      { remaining : int
      ; step : char
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let completion_time step =
    let step_time = 60 in
    step_time + Char.to_int step - Char.to_int 'A' + 1
  ;;

  let create step =
    { remaining = completion_time step
    ; step
    }
  ;;
end

let find_time_to_complete_steps dag ~workers_count =
  let complete_worker workers worker =
    Worker.Set.remove workers worker
    |> Worker.Set.map ~f:(fun other_worker ->
      { other_worker with
        remaining = other_worker.remaining - worker.remaining
      })
  in
  let rec helper ~time ~steps ~workers =
    if Set.is_empty steps && Set.is_empty workers
    then time
    else (
      let time, steps, workers =
        (* Rely on the fact that [workers] is ordered. *)
        Set.min_elt workers
        |> Option.value_map
          ~default:(time, steps, workers)
          ~f:(fun worker ->
            let { Worker.remaining; step } = worker in
            let time = time + remaining in
            let workers = complete_worker workers worker in
            let steps = Set.union steps (new_sources dag step) in
            time, steps, workers)
      in
      let steps, workers =
        Set.fold steps
          ~init:(Char.Set.empty, workers)
          ~f:(fun (remaining_steps, workers) step ->
            if Int.( >= ) (Set.length workers) workers_count
            then Set.add remaining_steps step, workers
            else remaining_steps, Set.add workers (Worker.create step))
      in
      helper ~time ~steps ~workers)
  in
  helper ~time:0 ~steps:(sources dag) ~workers:Worker.Set.empty
;;

let part_2 file_name ~workers =
  let%map dag = read_dag file_name in
  let time_to_complete_steps =
    find_time_to_complete_steps dag ~workers_count:workers
  in
  printf "%d\n" time_to_complete_steps
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
     and workers =
       flag "-workers" (optional_with_default 5 int)
         ~doc:"WORKERS number of workers (default: 5)"
     in
     fun () -> part_2 input_file ~workers)
;;

let command =
  Command.group
    ~summary:"Day 7"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
