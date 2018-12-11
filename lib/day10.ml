open! Core
open! Async
open Utils

module Point = struct
  module T = struct
    type t =
      { pos_x : int
      ; pos_y : int
      ; vel_x : int
      ; vel_y : int
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let re =
    let open Tyre in
    [%tyre "position=< *(?&pos_x:int),  *(?&pos_y:int)> \
            velocity=< *(?&vel_x:int),  *(?&vel_y:int)>"]
    |> compile
  ;;

  let of_object obj =
    { pos_x = obj#pos_x
    ; pos_y = obj#pos_y
    ; vel_x = obj#vel_x
    ; vel_y = obj#vel_y
    }
  ;;

  let tick t =
    { t with pos_x = t.pos_x + t.vel_x; pos_y = t.pos_y + t.vel_y }
  ;;

  let dist { pos_x = x1; pos_y = y1; _ } { pos_x = x2; pos_y = y2; _ } =
    Int.(abs (x2 - x1) + abs (y2 - y1))
  ;;

  let is_next_to t other_t =
    let dist = dist t other_t in
    Int.(dist > 0 && dist <= 5)
  ;;
end

let read_points file_name =
  let%map point_strings = Reader.file_lines file_name in
  List.map point_strings ~f:(fun point_string ->
    exec_re Point.re point_string |> Point.of_object)
  |> Point.Set.of_list
;;

let has_letters points =
  Set.for_all points
    ~f:(fun point -> Set.exists points ~f:(Point.is_next_to point))
;;

let rec find_letters points i =
  if has_letters points
  then (
    print_s [%message "Seconds" (i : int)];
    points)
  else (
    let points = Point.Set.map points ~f:Point.tick in
    find_letters points (i + 1))
;;

let plot_points points =
  Graphics.open_graph "";
  Graphics.set_window_title "Letters";
  let min_x, min_y =
    Point.Set.fold points
      ~init:(Int.max_value, Int.max_value)
      ~f:(fun (min_x, min_y) point ->
        let min_x = Int.min min_x point.pos_x in
        let min_y = Int.min min_y point.pos_y in
        min_x, min_y)
  in
  let max_x, max_y =
    Point.Set.fold points
      ~init:(Int.min_value, Int.min_value)
      ~f:(fun (max_x, max_y) point ->
        let max_x = Int.max max_x point.pos_x in
        let max_y = Int.max max_y point.pos_y in
        max_x, max_y)
  in
  Graphics.resize_window (max_x - min_x + 3) (max_y - min_y + 3);
  Point.Set.iter points ~f:(fun point ->
    Graphics.plot (point.pos_x - min_x + 1) (max_y - point.pos_y + 1));
  Deferred.never ()
;;

let main file_name =
  let%bind points = read_points file_name in
  let points = find_letters points 0 in
  plot_points points
;;

let command =
  Command.async
    ~summary:"Day 10"
    (let open Command.Let_syntax in
     let%map_open input_file =
       flag "-input" (required file) ~doc:"FILE path to input file"
     in
     fun () -> main input_file)
;;

let () = Command.run command
