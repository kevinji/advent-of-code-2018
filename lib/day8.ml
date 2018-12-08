open! Core
open! Async
open Utils

module Node = struct
  type t =
    { children : t list
    ; metadata : int list
    }
  [@@deriving sexp]

  let rec metadata_sum t =
    List.sum (module Int) t.metadata ~f:Fn.id
    + List.sum (module Int) t.children ~f:metadata_sum
  ;;

  module Int_counter = Make_counter (Int)

  let rec value t =
    if List.is_empty t.children
    then List.sum (module Int) t.metadata ~f:Fn.id
    else (
      let child_counter = Int_counter.of_list t.metadata in
      List.foldi t.children
        ~init:0
        ~f:(fun i sum child ->
          Map.find child_counter (i + 1)
          |> Option.value_map ~default:sum ~f:(fun freq ->
            sum + freq * value child)))
  ;;
end

let read_tokens file_name =
  let%map tokens_string =
    match%map Reader.file_lines file_name with
    | [ tokens_string ] -> tokens_string
    | [] -> raise_s [%message "No tokens found"]
    | _ :: _ -> raise_s [%message "More than one line found"]
  in
  let tokens =
    String.split tokens_string ~on:' '
    |> List.map ~f:Int.of_string
  in
  tokens
;;

let make_tree tokens =
  let rec helper tokens =
    match tokens with
    | [] | [ _ ] -> raise_s [%message "Tokens unexpectedly empty"]
    | children_count :: metadata_count :: tokens ->
      let tokens, children_rev =
        Sequence.range 0 children_count
        |> Sequence.fold ~init:(tokens, [])
          ~f:(fun (tokens, children_rev) (_ : int) ->
            let tokens, node = helper tokens in
            tokens, node :: children_rev)
      in
      let children = List.rev children_rev in
      let tokens, metadata_rev =
        Sequence.range 0 metadata_count
        |> Sequence.fold ~init:(tokens, [])
          ~f:(fun (tokens, metadata_rev) (_ : int) ->
            match tokens with
            | [] -> raise_s [%message "Tokens unexpectedly empty"]
            | token :: tokens ->
              tokens, token :: metadata_rev)
      in
      let metadata = List.rev metadata_rev in
      let node = { Node.children; metadata } in
      tokens, node
  in
  match helper tokens with
  | [], node -> node
  | _ :: _, _ -> raise_s [%message "Leftover tokens remaining"]
;;

let part_1 file_name =
  let%map tokens = read_tokens file_name in
  let root = make_tree tokens in
  let metadata_sum = Node.metadata_sum root in
  printf "%d\n" metadata_sum
;;

let part_2 file_name =
  let%map tokens = read_tokens file_name in
  let root = make_tree tokens in
  let root_value = Node.value root in
  printf "%d\n" root_value
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
    ~summary:"Day 8"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
