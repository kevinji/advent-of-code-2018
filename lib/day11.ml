open! Core
open! Async
open! Incremental_kernel
open! Utils

let read_serial_number file_name =
  let%map serial_number =
    match%map Reader.file_lines file_name with
    | [ serial_number ] -> serial_number
    | [] -> raise_s [%message "No serial number found"]
    | _ :: _ -> raise_s [%message "More than one line found"]
  in
  Int.of_string serial_number
;;

module Incr : Incremental.S = Incremental.Make ()

(* (x, y) need to be translated up by 1. *)
let power_level ~serial_number x_var y_var =
  let open Incr.Let_syntax in
  let%bind x = Incr.Var.watch x_var in
  let rack_id = (x + 1) + 10 in
  let%map y = Incr.Var.watch y_var in
  let power_level = (rack_id * (y + 1) + serial_number) * rack_id in
  let power_level = (power_level mod 1000) / 100 in
  power_level - 5
;;

let length = 300

let build_power_grid ~serial_number =
  let grid = Array.make_matrix ~dimx:length ~dimy:length 0 in
  let x_var = Incr.Var.create 0 in
  let y_var = Incr.Var.create 0 in
  let power_level_obs = Incr.observe (power_level ~serial_number x_var y_var) in
  Sequence.iter (Sequence.range 0 length) ~f:(fun x ->
    Sequence.iter (Sequence.range 0 length) ~f:(fun y ->
      Incr.Var.set x_var x;
      Incr.Var.set y_var y;
      Incr.stabilize ();
      let power_level = Incr.Observer.value_exn power_level_obs in
      grid.(x).(y) <- power_level));
  grid
;;

(* Avoid recalculating sums by precomputing 1 x slice (width x height) sums. *)
let build_y_slices grid ~slice =
  let slice_sums = Array.make_matrix ~dimx:length ~dimy:(length - slice + 1) 0 in
  Sequence.iter (Sequence.range 0 length) ~f:(fun x ->
    Sequence.iter (Sequence.range 0 (length - slice + 1)) ~f:(fun y ->
      let grid_slice = Array.slice grid.(x) y (y + slice) in
      slice_sums.(x).(y) <- Array.sum (module Int) grid_slice ~f:Fn.id));
  slice_sums
;;

let total_power slice_sums_tr ~slice (x, y) =
  let slice_sums_slice = Array.slice slice_sums_tr.(y) x (x + slice) in
  Array.sum (module Int) slice_sums_slice ~f:Fn.id
;;

let min_sum = -45 (* 9 cells, min -5 power level *)

let find_coord_with_most_power grid ~slice =
  let slice_sums_tr = build_y_slices grid ~slice |> Array.transpose_exn in
  Sequence.fold (Sequence.range 0 (length - slice + 1))
    ~init:((0, 0), min_sum)
    ~f:(fun (best_coord, best_sum) x ->
      Sequence.fold (Sequence.range 0 (length - slice + 1))
        ~init:(best_coord, best_sum)
        ~f:(fun (best_coord, best_sum) y ->
          let curr_sum = total_power slice_sums_tr ~slice (x, y) in
          if curr_sum > best_sum
          then (x, y), curr_sum
          else best_coord, best_sum))
;;

let part_1 file_name =
  let%map serial_number = read_serial_number file_name in
  let power_grid = build_power_grid ~serial_number in
  let (x, y), (_ : int) = find_coord_with_most_power power_grid ~slice:3 in
  printf "(%d, %d) \n" (x + 1) (y + 1)
;;

let part_2 file_name =
  let%map serial_number = read_serial_number file_name in
  let power_grid = build_power_grid ~serial_number in
  let (x, y), slice, _ =
    Sequence.fold (Sequence.range 1 length)
      ~init:((0, 0), 0, min_sum)
      ~f:(fun (best_coord, best_slice, best_sum) curr_slice ->
        let curr_coord, curr_sum =
          find_coord_with_most_power power_grid ~slice:curr_slice
        in
        if curr_sum > best_sum
        then curr_coord, curr_slice, curr_sum
        else best_coord, best_slice, best_sum)
  in
  printf "(%d, %d), size %d \n" (x + 1) (y + 1) slice
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
    ~summary:"Day 11"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
