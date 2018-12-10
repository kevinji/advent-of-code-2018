open! Core
open! Async
open Utils

module Action = struct
  type t =
    | Begins_shift of int
    | Falls_asleep
    | Wakes_up
  [@@deriving sexp, compare]

  let begins_shift_re =
    let open Tyre in
    [%tyre "Guard #(?&guard_id:pos_int) begins shift"]
    |> compile
  ;;
end

module Record = struct
  module T = struct
    type t =
      { time : Time.t
      ; action : Action.t
      }
    [@@deriving sexp, compare, fields]
  end

  include T
  include Comparable.Make (T)
end

let action_log_re =
  let open Tyre in
  [%tyre "\\[(?<time>[^\\]]+)\\] (?<action>.+)"]
  |> compile
;;

let read_records file_name =
  let%map action_logs = Reader.file_lines file_name in
  List.map action_logs ~f:(fun action_log ->
    let action_log_obj = exec_re action_log_re action_log in
    let time =
      Time.of_string_gen action_log_obj#time
        ~if_no_timezone:(`Use_this_one Time.Zone.utc)
    in
    let action : Action.t =
      match action_log_obj#action with
      | "wakes up" -> Wakes_up
      | "falls asleep" -> Falls_asleep
      | action ->
        let guard_id = exec_re Action.begins_shift_re action in
        Begins_shift guard_id
    in
    { Record.time; action })
  |> List.sort ~compare:Record.compare
;;

module Sleep_stats = struct
  type t =
    { total_sleep : Time.Span.t
    ; sleep_by_min : Time.Span.t Int.Map.t
    }
  [@@deriving sexp, fields]

  let empty =
    { total_sleep = Time.Span.zero
    ; sleep_by_min = Int.Map.empty
    }
  ;;

  let rec yield_mins ~start_time ~end_time =
    let open Sequence.Generator in
    let open Let_syntax in
    if Time.Ofday.(>=) start_time end_time
    then return ()
    else (
      let minute = (Time.Ofday.to_parts start_time).min in
      let%bind () = yield minute in
      let start_time =
        Time.Ofday.add start_time Time.Span.minute |> Option.value_exn
      in
      yield_mins ~start_time ~end_time)
  ;;

  let add t ~start_time ~end_time =
    let sleep = Time.diff end_time start_time in
    let total_sleep = Time.Span.(+) t.total_sleep sleep in
    let mins_seq =
      let start_time = Time.to_ofday start_time ~zone:Time.Zone.utc in
      let end_time = Time.to_ofday end_time ~zone:Time.Zone.utc in
      Sequence.Generator.run (yield_mins ~start_time ~end_time)
    in
    let sleep_by_min =
      Sequence.fold mins_seq ~init:t.sleep_by_min
        ~f:(fun sleep_by_min min ->
            Map.update sleep_by_min min ~f:(function
              | None -> Time.Span.minute
              | Some sleep -> Time.Span.(sleep + minute)))
    in
    { total_sleep; sleep_by_min }
  ;;
end

module Asleep_times = struct
  type t =
    { sleep_by_guard : Sleep_stats.t Int.Map.t
    ; guard_id : int
    ; last_record_time : Time.t
    }
  [@@deriving sexp]
end

let find_asleep_times records =
  let helper asleep_times record =
    let { Asleep_times.sleep_by_guard; guard_id; last_record_time } =
      asleep_times
    in
    let record_time = Record.time record in
    let asleep_times =
      match Record.action record with
      | Begins_shift guard_id -> { asleep_times with guard_id }
      | Falls_asleep -> asleep_times
      | Wakes_up ->
        let sleep_by_guard =
          Map.update sleep_by_guard guard_id
            ~f:(fun old_sleep ->
              let old_sleep =
                Option.value old_sleep ~default:Sleep_stats.empty
              in
              Sleep_stats.add
                old_sleep
                ~start_time:last_record_time
                ~end_time:record_time)
        in
        { asleep_times with sleep_by_guard }
    in
    { asleep_times with last_record_time = record_time }
  in
  let asleep_times =
    match records with
    | record :: records ->
      (match Record.action record with
       | Begins_shift guard_id ->
         let init =
           { Asleep_times.
             sleep_by_guard = Int.Map.empty
           ; guard_id
           ; last_record_time = Record.time record
           }
         in
         List.fold records ~init ~f:helper
       | Wakes_up | Falls_asleep ->
         raise_s [%message "List of records did not start with Begins_shift"]
      )
    | [] ->
      raise_s [%message "No records found; this should never happen"]
  in
  asleep_times.sleep_by_guard
;;

let part_1 file_name =
  let%map records = read_records file_name in
  let sleep_by_guard = find_asleep_times records in
  let guard_with_most_sleep, sleep_stats =
    Map.fold sleep_by_guard ~init:(None, Sleep_stats.empty)
      ~f:(fun
           ~key:guard_id
           ~data:sleep_stats
           (max_guard_id, max_sleep_stats) ->
        let sleep = Sleep_stats.total_sleep sleep_stats in
        let max_sleep = Sleep_stats.total_sleep max_sleep_stats in
        if Time.Span.(>) sleep max_sleep
        then Some guard_id, sleep_stats
        else max_guard_id, max_sleep_stats)
  in
  let guard_with_most_sleep = Option.value_exn guard_with_most_sleep in
  let min_with_most_sleep, (_ : Time.Span.t) =
    Map.fold (Sleep_stats.sleep_by_min sleep_stats)
      ~init:(None, Time.Span.zero)
      ~f:(fun ~key:min ~data:sleep (max_min, max_sleep) ->
        if Time.Span.(>) sleep max_sleep
        then Some min, sleep
        else max_min, max_sleep)
  in
  let min_with_most_sleep = Option.value_exn min_with_most_sleep in
  printf "%d\n" (guard_with_most_sleep * min_with_most_sleep)
;;

let part_2 file_name =
  let%map records = read_records file_name in
  let sleep_by_guard = find_asleep_times records in
  let guard_with_most_sleep_on_min, min_with_most_sleep, (_ : Time.Span.t) =
    Map.fold sleep_by_guard ~init:(None, -1, Time.Span.zero)
      ~f:(fun
           ~key:guard_id
           ~data:sleep_stats
           (max_guard_id, max_min, max_sleep) ->
        Map.fold (Sleep_stats.sleep_by_min sleep_stats)
          ~init:(max_guard_id, max_min, max_sleep)
          ~f:(fun ~key:min ~data:sleep (max_guard_id, max_min, max_sleep) ->
            if Time.Span.(>) sleep max_sleep
            then Some guard_id, min, sleep
            else max_guard_id, max_min, max_sleep))
  in
  let guard_with_most_sleep_on_min =
    Option.value_exn guard_with_most_sleep_on_min
  in
  printf "%d\n" (guard_with_most_sleep_on_min * min_with_most_sleep)
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
    ~summary:"Day 4"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
