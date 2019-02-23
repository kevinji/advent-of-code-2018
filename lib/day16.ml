open! Core
open! Async
open! Utils

module Registers = struct
  type t = int array [@@deriving sexp, compare]

  let re =
    Tyre.(separated_list ~sep:(str ", ") int)
    |> Tyre.conv Array.of_list Array.to_list
  ;;

  let init () = [| 0; 0; 0; 0 |]
end

module Instruction = struct
  (** Form: [opcode] A B C *)
  type t =
    { opcode : int
    ; input_a : int
    ; input_b : int
    ; output : int
    }
  [@@deriving sexp, compare]

  let re =
    let open Tyre in
    [%tyre "(?&opcode:int) (?&input_a:int) (?&input_b:int) (?&output:int)"]
    |> compile
  ;;

  let of_obj obj =
    { opcode = obj#opcode
    ; input_a = obj#input_a
    ; input_b = obj#input_b
    ; output = obj#output
    }
  ;;

  let of_line line = exec_re re line |> of_obj
end

module Opcode = struct
  type t = Registers.t -> Instruction.t -> Registers.t Or_error.t

  let run registers ~output ~data =
    let registers = Array.copy registers in
    registers.(output) <- data;
    registers
  ;;

  let rr op registers { Instruction.input_a; input_b; output; _ } =
    Or_error.try_with (fun () ->
      run registers ~output ~data:(op registers.(input_a) registers.(input_b)))
  ;;

  let ri op registers { Instruction.input_a; input_b; output; _ } =
    Or_error.try_with (fun () ->
      run registers ~output ~data:(op registers.(input_a) input_b))
  ;;

  let ir op registers { Instruction.input_a; input_b; output; _ } =
    Or_error.try_with (fun () ->
      run registers ~output ~data:(op input_a registers.(input_b)))
  ;;

  let addr = rr ( + )
  let addi = ri ( + )

  let mulr = rr ( * )
  let muli = ri ( * )

  let banr = rr ( land )
  let bani = ri ( land )

  let borr = rr ( lor )
  let bori = ri ( lor )

  let setr registers { Instruction.input_a; output; _ } =
    Or_error.try_with (fun () ->
      run registers ~output ~data:registers.(input_a))
  ;;

  let seti registers { Instruction.input_a; output; _ } =
    Or_error.try_with (fun () -> run registers ~output ~data:input_a)
  ;;

  let bool_op op a b = op a b |> Bool.to_int

  let gtir = ir (bool_op Int.( > ))
  let gtri = ri (bool_op Int.( > ))
  let gtrr = rr (bool_op Int.( > ))

  let eqir = ir (bool_op Int.( = ))
  let eqri = ri (bool_op Int.( = ))
  let eqrr = rr (bool_op Int.( = ))

  let all : t String.Map.t =
    [ "addr", addr
    ; "addi", addi
    ; "mulr", mulr
    ; "muli", muli
    ; "banr", banr
    ; "bani", bani
    ; "borr", borr
    ; "bori", bori
    ; "setr", setr
    ; "seti", seti
    ; "gtir", gtir
    ; "gtri", gtri
    ; "gtrr", gtrr
    ; "eqir", eqir
    ; "eqri", eqri
    ; "eqrr", eqrr
    ]
    |> String.Map.of_alist_exn
  ;;
end

module Sample = struct
  module T = struct
    type t =
      { registers_before : Registers.t
      ; instruction : Instruction.t
      ; registers_after : Registers.t
      }
    [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)

  let before_re =
    let open Tyre in
    [%tyre "Before:\\s+\\[(?&Registers.re)\\]"] |> compile
  ;;

  let after_re =
    let open Tyre in
    [%tyre "After:\\s+\\[(?&Registers.re)\\]"] |> compile
  ;;

  let of_lines ~before_line ~instruction_line ~after_line =
    let registers_before = exec_re before_re before_line in
    let instruction = Instruction.of_line instruction_line in
    let registers_after = exec_re after_re after_line in
    { registers_before; instruction; registers_after }
  ;;

  let behaves_like_opcode t opcode =
    match opcode t.registers_before t.instruction with
    | Ok registers_after ->
      Array.equal t.registers_after registers_after ~equal:Int.equal
    | Error (_ : Error.t) -> false
  ;;

  let behaves_like_at_least_n_opcodes t ~n =
    let opcode_count =
      String.Map.data Opcode.all |> List.count ~f:(behaves_like_opcode t)
    in
    Int.(opcode_count >= n)
  ;;
end

let parse_samples lines =
  let rec helper ~samples_rev ~lines =
    match lines with
    | "" :: "" :: rest ->
      List.rev samples_rev, rest
    | before_line :: instruction_line :: after_line :: "" :: rest ->
      let samples_rev =
        let sample =
          Sample.of_lines ~before_line ~instruction_line ~after_line
        in
        sample :: samples_rev
      in
      helper ~samples_rev ~lines:rest
    | _ ->
      raise_s [%message "Unexpected number of lines" (lines : string list)]
  in
  helper ~samples_rev:[] ~lines
;;

let parse_program lines =
  let rec helper ~instructions_rev ~lines =
    match lines with
    | [] -> List.rev instructions_rev
    | line :: rest ->
      let instructions_rev =
        let instruction = Instruction.of_line line in
        instruction :: instructions_rev
      in
      helper ~instructions_rev ~lines:rest
  in
  helper ~instructions_rev:[] ~lines
;;

let read_samples_and_program file_name =
  let%map lines = Reader.file_lines file_name in
  let samples, lines = parse_samples lines in
  let program = parse_program lines in
  samples, program
;;

let part_1 file_name =
  let%map samples, (_ : Instruction.t list) =
    read_samples_and_program file_name
  in
  let samples_that_behave_like_3_or_more_opcodes =
    List.count samples ~f:(Sample.behaves_like_at_least_n_opcodes ~n:3)
  in
  printf "Samples that behave like 3 or more opcodes: %d\n"
    samples_that_behave_like_3_or_more_opcodes
;;

let match_opcodes_with_numbers samples =
  let rec helper ~opcode_by_number ~remaining_opcodes ~samples =
    let sample, opcode_name, opcode =
      let remaining_opcodes_list = Map.to_alist remaining_opcodes in
      Set.find_map samples ~f:(fun sample ->
        let open Option.Let_syntax in
        let%map opcode_name, opcode =
          List.fold_until
            remaining_opcodes_list
            ~init:None
            ~f:(fun result_opt (opcode_name, opcode) ->
                match result_opt, Sample.behaves_like_opcode sample opcode with
                | Some _, true -> Stop None
                | None, true -> Continue (Some (opcode_name, opcode))
                | _, false -> Continue result_opt)
            ~finish:Fn.id
        in
        sample, opcode_name, opcode)
      |> Option.value_exn
           ~here:[%here]
           ~message:"Error matching all opcodes with numbers"
    in
    let remaining_opcodes = Map.remove remaining_opcodes opcode_name in
    let opcode_number = sample.instruction.opcode in
    let samples =
      Set.filter samples ~f:(fun sample ->
        Int.( <> ) sample.instruction.opcode opcode_number)
    in
    let opcode_by_number =
      Map.add_exn opcode_by_number ~key:opcode_number ~data:opcode
    in
    if Map.is_empty remaining_opcodes
    then opcode_by_number
    else helper ~opcode_by_number ~remaining_opcodes ~samples
  in
  helper
    ~opcode_by_number:Int.Map.empty
    ~remaining_opcodes:Opcode.all
    ~samples:(Sample.Set.of_list samples)
;;

let part_2 file_name =
  let%map samples, program = read_samples_and_program file_name in
  let opcode_by_number = match_opcodes_with_numbers samples in
  let final_registers =
    List.fold
      program
      ~init:(Registers.init ())
      ~f:(fun registers instruction ->
        let opcode_number = instruction.opcode in
        let opcode = Map.find_exn opcode_by_number opcode_number in
        opcode registers instruction |> Or_error.ok_exn)
  in
  print_s [%message "Final registers" (final_registers : Registers.t)];
  printf "Register 0: %d\n" (final_registers.(0))
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
    ~summary:"Day 16"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
