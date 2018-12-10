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
