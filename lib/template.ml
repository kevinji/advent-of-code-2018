open! Core
open! Async
open! Utils

let read_lines file_name =
  let%map lines = Reader.file_lines file_name in
  lines
;;

let part_1 file_name =
  let%map lines = read_lines file_name in
  printf "%d\n" (List.length lines)
;;

let part_2 file_name =
  let%map lines = read_lines file_name in
  printf "%d\n" (List.length lines)
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
    ~summary:"Day FIXME"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
