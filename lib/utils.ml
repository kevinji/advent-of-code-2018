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
