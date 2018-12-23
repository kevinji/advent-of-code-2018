open! Core
open! Async
open! Utils

module Turn_dir = struct
  type t = Left | Straight | Right [@@deriving sexp, compare]

  let next = function
    | Left -> Straight
    | Straight -> Right
    | Right -> Left
  ;;
end

module Dir = struct
  type t = Up | Down | Left | Right [@@deriving sexp, compare]

  let rotate_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up
  ;;

  let rotate_left = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up
  ;;

  let turn t (turn_dir : Turn_dir.t) =
    match t, turn_dir with
    | Up, Left | Left, Straight | Down, Right -> Left
    | Right, Left | Up, Straight | Left, Right -> Up
    | Down, Left | Right, Straight | Up, Right -> Right
    | Left, Left | Down, Straight | Right, Right -> Down
  ;;
end

module Pos = struct
  type t =
    { y : int
    ; x : int
    }
  [@@deriving sexp, compare, fields]

  let to_string { y; x } = sprintf "(%d, %d)" x y

  let next { y; x } = function
    | Dir.Up -> { y = y - 1; x }
    | Down   -> { y = y + 1; x }
    | Left   -> { y; x = x - 1 }
    | Right  -> { y; x = x + 1 }
  ;;
end

module Tracks = struct
  type t = char array array [@@deriving sexp]

  let get t { Pos.y; x } = t.(y).(x)
end

module Cart = struct
  module T = struct
    type t =
      { pos : Pos.t
      ; dir : Dir.t
      ; turn_dir : Turn_dir.t
      }
    [@@deriving sexp]

    let compare t1 t2 = Pos.compare t1.pos t2.pos
  end

  include T
  include Comparable.Make (T)

  let chars = Char.Set.of_list [ '^'; 'v'; '<'; '>' ]

  let create pos ch =
    let dir : Dir.t =
      match ch with
      | '^' -> Up
      | 'v' -> Down
      | '<' -> Left
      | '>' -> Right
      | _ -> raise_s [%message "Invalid cart char" (ch : char)]
    in
    { pos; dir; turn_dir = Left }
  ;;

  let move { pos; dir; turn_dir } tracks =
    let pos = Pos.next pos dir in
    let dir, turn_dir =
      match Tracks.get tracks pos, dir with
      | '|', (Up | Down)
      | '-', (Left | Right) -> dir, turn_dir
      | '/', (Up | Down)
      | '\\', (Left | Right) -> Dir.rotate_right dir, turn_dir
      | '/', (Left | Right)
      | '\\', (Up | Down) -> Dir.rotate_left dir, turn_dir
      | '+', _ -> Dir.turn dir turn_dir, Turn_dir.next turn_dir
      | _ as track, _ ->
        raise_s
          [%message "Invalid track and current dir"
              (track : char)
              (dir : Dir.t)]
    in
    { pos; dir; turn_dir }
  ;;
end

module Board = struct
  type t =
    { tracks : Tracks.t
    ; carts : Cart.Set.t
    }
  [@@deriving sexp]

  let of_lines lines =
    let tracks = Array.of_list_map lines ~f:String.to_array in
    let carts_with_pos =
      Array.concat_mapi tracks ~f:(fun y row ->
        Array.filter_mapi row ~f:(fun x ch ->
          Option.some_if (Set.mem Cart.chars ch) ({ Pos.y; x }, ch)))
    in
    Array.iter carts_with_pos ~f:(fun ({ y; x }, cart_ch) ->
      let track_ch =
        match cart_ch with
        | '^' | 'v' -> '|'
        | '<' | '>' -> '-'
        | _ -> raise_s [%message "Invalid cart char" (cart_ch : char)]
      in
      tracks.(y).(x) <- track_ch);
    let carts =
      Array.map carts_with_pos ~f:(fun (pos, cart_ch) ->
        Cart.create pos cart_ch)
      |> Cart.Set.of_array
    in
    { tracks; carts }
  ;;

  let rec tick_until_collision t =
    Set.fold_until t.carts
      ~init:t.carts
      ~f:(fun carts cart ->
        let carts = Set.remove carts cart in
        let cart = Cart.move cart t.tracks in
        let has_collided = Set.mem carts cart in
        if has_collided
        then Stop cart.pos
        else Continue (Set.add carts cart))
      ~finish:(fun carts -> tick_until_collision { t with carts })
  ;;

  let rec tick_until_last_cart t =
    let carts, (_removed_carts : Cart.Set.t) =
      Set.fold t.carts
        ~init:(t.carts, Cart.Set.empty)
        ~f:(fun (carts, removed_carts) cart ->
          if Set.mem removed_carts cart
          then carts, removed_carts
          else (
            let carts = Set.remove carts cart in
            let cart = Cart.move cart t.tracks in
            let has_collided = Set.mem carts cart in
            if has_collided
            then Set.remove carts cart, Set.add removed_carts cart
            else Set.add carts cart, removed_carts))
    in
    if Int.(<=) (Set.length carts) 1
    then Set.min_elt_exn carts
    else tick_until_last_cart { t with carts }
  ;;
end

let read_board file_name =
  let%map lines = Reader.file_lines file_name in
  Board.of_lines lines
;;

let part_1 file_name =
  let%map board = read_board file_name in
  let collision_pos = Board.tick_until_collision board in
  printf !"%{Pos}\n" collision_pos
;;

let part_2 file_name =
  let%map board = read_board file_name in
  let last_cart = Board.tick_until_last_cart board in
  printf !"%{Pos}\n" last_cart.pos
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
    ~summary:"Day 13"
    [ "part-1", cmd_part_1; "part-2", cmd_part_2 ]
;;

let () = Command.run command
