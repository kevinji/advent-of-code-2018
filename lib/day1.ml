open! Core
open! Async
open! Utils

let read_freq_changes file_name =
  let%map freq_changes = Reader.file_lines file_name in
  List.map freq_changes ~f:Int.of_string
;;

let part_1 file_name =
  let%map freq_changes = read_freq_changes file_name in
  let final_freq = List.sum (module Int) freq_changes ~f:Fn.id in
  printf "%d\n" final_freq
;;

let find_duplicate_freq (seen_freqs, curr_freq) freq_change
  : _ Base.Continue_or_stop.t
  =
  let new_freq = curr_freq + freq_change in
  if Set.mem seen_freqs new_freq
  then Stop new_freq
  else (
    let seen_freqs = Set.add seen_freqs new_freq in
    Continue (seen_freqs, new_freq))
;;

let part_2 file_name =
  let%map freq_changes = read_freq_changes file_name in
  let freq_changes_cycle = Sequence.cycle_list_exn freq_changes in
  let duplicate_freq =
    Sequence.fold_until freq_changes_cycle
      ~init:((Int.Set.singleton 0), 0)
      ~f:find_duplicate_freq
      ~finish:(fun (_, _) ->
        raise_s
          [%message "Frequency cycle terminated; this should never happen"])
  in
  printf "%d\n" duplicate_freq
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
    ~summary:"Day 1"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
