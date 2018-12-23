open! Core
open! Async

module Make_counter (Key : Comparable.S) = struct
  let of_list list =
    List.fold list
      ~init:Key.Map.empty
      ~f:(fun t ele ->
        Map.update t ele ~f:(function
          | None -> 1
          | Some freq -> freq + 1))
  ;;
end

let exec_re re string =
  match Tyre.exec re string with
  | Ok value -> value
  | Error (`NoMatch (_, _)) ->
    raise_s [%message "tyre does not match string"]
  | Error (`ConverterFailure exn) -> raise exn
;;

let read_one_line file_name =
  match%map Reader.file_lines file_name with
  | [ line ] -> line
  | [] -> raise_s [%message "No string found"]
  | _ :: _ -> raise_s [%message "More than one line found"]
;;
