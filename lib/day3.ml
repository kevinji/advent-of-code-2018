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
end

module Claim = struct
  type t =
    { id : int
    ; top_left_x : int
    ; top_left_y : int
    ; width : int
    ; height : int
    }
  [@@deriving sexp]

  let re =
    let open Tyre in
    [%tyre "#(?&id:pos_int) @ (?&x:pos_int),(?&y:pos_int): \
            (?&width:pos_int)x(?&height:pos_int)"]
    |> compile
  ;;

  let of_object obj =
    { id = obj#id
    ; top_left_x = obj#x
    ; top_left_y = obj#y
    ; width = obj#width
    ; height = obj#height
    }
  ;;

  let points t =
    let open Sequence.Let_syntax in
    let%bind y = Sequence.range t.top_left_y (t.top_left_y + t.height) in
    let%map x = Sequence.range t.top_left_x (t.top_left_x + t.width) in
    { Point.x; y }
  ;;
end

let read_claims file_name =
  let%map claims = Reader.file_lines file_name in
  List.map claims ~f:(fun claim -> Claim.of_object (exec_re Claim.re claim))
;;

let find_multi_points claims =
  let _covered_points, multi_points =
    List.fold claims
      ~init:(Point.Set.empty, Point.Set.empty)
      ~f:(fun (covered_points, multi_points) claim ->
        let points = Claim.points claim in
        let multi_points =
          Sequence.fold points ~init:multi_points
            ~f:(fun multi_points point ->
              if Set.mem covered_points point
              then Set.add multi_points point
              else multi_points)
        in
        let covered_points =
          Sequence.fold points ~init:covered_points ~f:Set.add
        in
        covered_points, multi_points)
  in
  multi_points
;;

let part_1 file_name =
  let%map claims = read_claims file_name in
  let multi_points = find_multi_points claims in
  printf "%d\n" (Set.length multi_points)
;;

let is_non_overlapping multi_points claim =
  let points = Claim.points claim in
  Sequence.for_all points ~f:(fun point -> not (Set.mem multi_points point))
;;

let part_2 file_name =
  let%map claims = read_claims file_name in
  let multi_points = find_multi_points claims in
  let non_overlapping_claim =
    List.find_exn claims ~f:(is_non_overlapping multi_points)
  in
  printf "%d\n" non_overlapping_claim.id
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
    ~summary:"Day 3"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
