open! Core
open! Async
open! Utils

module Point = struct
  module T = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let of_string str =
    let parts = String.split_on_chars str ~on:[ ','; ' ' ] in
    match parts with
    | [ x_str; ""; y_str ] ->
      { x = Int.of_string x_str
      ; y = Int.of_string y_str
      }
    | _ -> raise_s [%message "Point was not in x, y format" str]
  ;;

  module O = struct
    let ( + ) { x = x1; y = y1 } { x = x2; y = y2 } =
      { x = x1 + x2; y = y1 + y2 }
    ;;

    let ( - ) { x = x1; y = y1 } { x = x2; y = y2 } =
      { x = x1 - x2; y = y1 - y2 }
    ;;
  end

  let min { x = x1; y = y1 } { x = x2; y = y2 } =
    { x = Int.min x1 x2; y = Int.min y1 y2 }
  ;;

  let max { x = x1; y = y1 } { x = x2; y = y2 } =
    { x = Int.max x1 x2; y = Int.max y1 y2 }
  ;;

  let shift t ~origin =
    { x = t.x - origin.x; y = t.y - origin.y }
  ;;

  let dist t1 t2 =
    let { x; y } = O.(t2 - t1) in
    Int.abs x + Int.abs y
  ;;

  include O
end

let read_points file_name =
  let%map point_strings = Reader.file_lines file_name in
  List.map point_strings ~f:Point.of_string
;;

let find_corners points =
  match points with
  | [] -> raise_s [%message "No points found"]
  | point :: points ->
    let top_left, bottom_right = point, point in
    List.fold points
      ~init:(top_left, bottom_right)
      ~f:(fun (top_left, bottom_right) point ->
        Point.min top_left point, Point.max bottom_right point)
;;

module Closest_point = struct
  type t =
    | Win of int
    | Tie
    | Unknown
  [@@deriving sexp_of, compare]
end

module Grid = struct
  type t =
    { closest : Closest_point.t array array
    ; bottom_right : Point.t
    }
  [@@deriving sexp_of]

  let of_bottom_right (bottom_right : Point.t) =
    let closest =
      Array.make_matrix
        ~dimx:(bottom_right.x + 1)
        ~dimy:(bottom_right.y + 1)
        Closest_point.Unknown
    in
    { closest; bottom_right }
  ;;

  let copy { closest; bottom_right } =
    { closest = Array.map closest ~f:Array.copy
    ; bottom_right
    }
  ;;

  let get t { Point.x; y } = t.closest.(x).(y)

  let set t { Point.x; y } value = t.closest.(x).(y) <- value

  let set_points t points =
    List.iteri points ~f:(fun i point -> set t point (Win i));
  ;;

  let in_bounds t (point : Point.t) =
    0 <= point.x
    && point.x <= t.bottom_right.x
    && 0 <= point.y
    && point.y <= t.bottom_right.y
  ;;

  let find_neighbors t (point : Point.t) =
    Point.
      [ point + { x = 1; y = 0 }
      ; point - { x = 1; y = 0 }
      ; point + { x = 0; y = 1 }
      ; point - { x = 0; y = 1 }
      ]
    |> List.filter ~f:(in_bounds t)
  ;;

  let find_status t (point : Point.t) =
    let neighbors = find_neighbors t point in
    List.fold neighbors
      ~init:Closest_point.Unknown
      ~f:(fun status neighbor ->
        match get t neighbor, status with
        | Unknown, _ -> status
        | Tie, _ -> Tie
        | Win i, Unknown -> Win i
        | Win _, Tie -> Tie
        | Win i, Win j ->
          if Int.(i = j)
          then Win i
          else Tie)
  ;;

  let find_candidates t (point : Point.t) =
    find_neighbors t point
    |> List.filter ~f:(fun neighbor ->
      match get t neighbor with
      | Unknown -> true
      | Win _ | Tie -> false)
  ;;

  let rec process t (candidates : Point.Set.t) =
    let new_t = copy t in
    Set.iter candidates ~f:(fun point ->
      set new_t point (find_status t point));
    let new_candidates =
      Set.fold candidates
        ~init:Point.Set.empty
        ~f:(fun new_candidates point ->
          List.fold (find_candidates new_t point)
            ~init:new_candidates
            ~f:Set.add);
    in
    if Set.is_empty new_candidates
    then new_t
    else process new_t new_candidates
  ;;

  let to_area_by_point t =
    Array.fold t.closest
      ~init:Int.Map.empty
      ~f:(fun area_by_point column ->
        Array.fold column
          ~init:area_by_point
          ~f:(fun area_by_point ->
            function
            | Tie | Unknown -> area_by_point
            | Win i ->
              Map.update area_by_point i
                ~f:(function
                    | None -> 1
                    | Some count -> count + 1)))
  ;;

  let points_on_edges t =
    let add_win_i points = function
      | Closest_point.Tie | Unknown -> points
      | Win i -> Set.add points i
    in
    let points =
      Array.fold t.closest.(0)
        ~init:Int.Set.empty
        ~f:add_win_i
    in
    let points =
      Array.fold t.closest.(Array.length t.closest - 1)
        ~init:points
        ~f:add_win_i
    in
    let closest = Array.transpose_exn t.closest in
    let points =
      Array.fold closest.(0)
        ~init:points
        ~f:add_win_i
    in
    let points =
      Array.fold closest.(Array.length closest - 1)
        ~init:points
        ~f:add_win_i
    in
    points
  ;;
end

let part_1 file_name =
  let%map points = read_points file_name in
  let top_left, bottom_right = find_corners points in
  let points = List.map points ~f:(Point.shift ~origin:top_left) in
  let bottom_right = Point.shift bottom_right ~origin:top_left in
  let grid = Grid.of_bottom_right bottom_right in
  Grid.set_points grid points;
  let candidates =
    List.concat_map points ~f:(Grid.find_candidates grid)
    |> Point.Set.of_list
  in
  let grid = Grid.process grid candidates in
  print_s [%message "" (grid.closest : Closest_point.t array array)];
  let area_by_point =
    let points_to_exclude = Grid.points_on_edges grid in
    Grid.to_area_by_point grid
    |> Map.filter_keys ~f:(Fn.non (Set.mem points_to_exclude))
  in
  let max_area =
    Map.fold area_by_point
      ~init:0
      ~f:(fun ~key:_ ~data:area best_area -> Int.max best_area area)
  in
  printf "%d\n" max_area
;;

let total_dist points point =
  List.sum (module Int) points ~f:(Point.dist point)
;;

let generate_points ~(bottom_right : Point.t) =
  let open List.Let_syntax in
  let%bind x = List.range ~stop:`inclusive 0 bottom_right.x in
  let%map y = List.range ~stop:`inclusive 0 bottom_right.y in
  { Point.x; y }
;;

let part_2 file_name ~max_dist =
  let%map points = read_points file_name in
  let top_left, bottom_right = find_corners points in
  let points = List.map points ~f:(Point.shift ~origin:top_left) in
  let bottom_right = Point.shift bottom_right ~origin:top_left in
  let points_under_max_dist =
    List.filter (generate_points ~bottom_right)
      ~f:(fun point -> total_dist points point < max_dist)
  in
  printf "%d\n" (List.length points_under_max_dist)
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
     and max_dist =
       flag "-max-dist" (optional_with_default 10000 int)
         ~doc:"DIST maximum distance from all points (default: 10000)"
     in
     fun () -> part_2 input_file ~max_dist)
;;

let command =
  Command.group
    ~summary:"Day 6"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
