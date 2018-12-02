open! Core
open! Async

let read_char_freqs_of_ids file_name =
  let%map ids = Reader.file_lines file_name in
  List.map ids ~f:(fun id ->
    String.fold id ~init:Char.Map.empty ~f:(fun char_freqs ch ->
      Map.update char_freqs ch ~f:(function
        | None -> 1
        | Some freq -> freq + 1)))
;;

let part_1 file_name =
  let%map char_freqs_of_ids = read_char_freqs_of_ids file_name in
  let ids_with_freq_2, ids_with_freq_3 =
    List.fold char_freqs_of_ids
      ~init:(0, 0)
      ~f:(fun (ids_with_freq_2, ids_with_freq_3) char_freqs ->
        let freq_2_count, freq_3_count =
          Map.fold char_freqs
            ~init:(0, 0)
            ~f:(fun ~key:_ ~data (freq_2_count, freq_3_count) ->
              match data with
              | 2 -> 1, freq_3_count
              | 3 -> freq_2_count, 1
              | _ -> freq_2_count, freq_3_count)
        in
        ids_with_freq_2 + freq_2_count, ids_with_freq_3 + freq_3_count)
  in
  let checksum = ids_with_freq_2 * ids_with_freq_3 in
  printf "%d\n" checksum
;;

let read_sorted_ids file_name =
  let%map ids = Reader.file_lines file_name in
  List.sort ~compare:String.compare ids
;;

let part_2 file_name =
  let%map sorted_ids = read_sorted_ids file_name in
  let id1, id2 =
    List.find_consecutive_duplicate sorted_ids
      ~equal:(fun id1 id2 ->
        let id1_chars = String.to_list id1 in
        let id2_chars = String.to_list id2 in
        let diff_chars =
          List.zip_exn id1_chars id2_chars
          |> List.count ~f:(fun (ch1, ch2) -> Char.(<>) ch1 ch2)
        in
        Int.(=) diff_chars 1)
    |> Option.value_exn ~here:[%here]
  in
  let matching_chars =
    let id1_chars = String.to_list id1 in
    let id2_chars = String.to_list id2 in
    List.zip_exn id1_chars id2_chars
    |> List.filter_map ~f:(fun (ch1, ch2) ->
        if Char.(=) ch1 ch2
        then Some ch1
        else None)
  in
  let common_chars = String.of_char_list matching_chars in
  printf "%s\n" common_chars
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
    ~summary:"Day 2"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
