open! Core
open! Async

let read_polymer file_name =
  let%map polymer =
    match%map Reader.file_lines file_name with
    | [ polymer ] -> polymer
    | [] -> raise_s [%message "Input file empty"]
    | _ :: _ -> raise_s [%message "Input file contained more than one line"]
  in
  String.to_list polymer
;;

let will_react unit next_unit =
  Char.(unit <> next_unit) && Char.(lowercase unit = lowercase next_unit)
;;

let rec react_polymer ~scanned ~remaining =
  match Fdeque.dequeue_front remaining with
  | None -> Fdeque.to_list scanned
  | Some (unit, remaining) ->
    match Fdeque.peek_front remaining with
    | None ->
      let scanned = Fdeque.enqueue_back scanned unit in
      react_polymer ~scanned ~remaining
    | Some next_unit ->
      if will_react unit next_unit
      then (
        (* [next_unit] must be at the front of [remaining]. *)
        let remaining = Fdeque.drop_front_exn remaining in
        match Fdeque.dequeue_back scanned with
        | None -> react_polymer ~scanned ~remaining
        | Some (prev_unit, scanned) ->
          let remaining = Fdeque.enqueue_front remaining prev_unit in
          react_polymer ~scanned ~remaining)
      else (
        let scanned = Fdeque.enqueue_back scanned unit in
        react_polymer ~scanned ~remaining)
;;

let length_of_final_polymer polymer_units =
  let final_polymer_units =
    let scanned = Fdeque.empty in
    let remaining = Fdeque.of_list polymer_units in
    react_polymer ~scanned ~remaining
  in
  List.length final_polymer_units
;;

let part_1 file_name =
  let%map polymer_units = read_polymer file_name in
  printf "%d\n" (length_of_final_polymer polymer_units)
;;

let part_2 file_name =
  let%map polymer_units = read_polymer file_name in
  let uniq_units = Char.Set.of_list polymer_units in
  let length_of_shortest_polymer =
    Set.fold uniq_units
      ~init:(List.length polymer_units)
      ~f:(fun shortest_length removed_unit ->
        let removed_units =
          Char.[ uppercase removed_unit; lowercase removed_unit ]
          |> Char.Set.of_list
        in
        let polymer_units =
          List.filter polymer_units ~f:(Fn.non (Set.mem removed_units))
        in
        let length = length_of_final_polymer polymer_units in
        Int.min length shortest_length)
  in
  printf "%d\n" length_of_shortest_polymer
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
    ~summary:"Day 5"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
