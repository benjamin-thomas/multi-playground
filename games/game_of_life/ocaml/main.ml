module R = Raylib
module Color = Raylib.Color

module Window = struct
  let w = 1000
  let h = 1000
end

module Cell = struct
  let size = 5
end

module Cells : sig
  val init : unit -> bool array array
  val iter : (int -> int -> unit) -> unit
  val rows : int
  val cols : int
end = struct
  let cols = Window.w / Cell.size
  let rows = Window.h / Cell.size

  let iter f =
    for x = 0 to rows - 1 do
      for y = 0 to cols - 1 do
        f x y
      done
    done
  ;;

  let init () =
    let m = Array.make_matrix cols rows false in
    ()
    ; iter (fun x y -> m.(x).(y) <- Random.bool ())
    ; m
  ;;
end

module Next_cells : sig
  val state : bool array array
end = struct
  let state =
    let cols = Window.w / Cell.size
    and rows = Window.h / Cell.size in
    Array.make_matrix cols rows false
  ;;
end

let draw_cell rect ~is_hovered ~is_alive ~alive_neigbours ~x ~y =
  ()
  ; if is_alive then
      R.draw_rectangle_rec rect Color.yellow
    else if is_hovered then
      R.draw_rectangle_rec rect Color.darkgray
    else
      R.draw_rectangle_rec rect Color.black
  ; R.draw_text
      (string_of_int alive_neigbours)
      ((x * Cell.size) + 7)
      ((y * Cell.size) + 3)
      20
      (Color.create 30 30 30 255)
;;

let check_player_state mouse_pos rect =
  let is_hovered = R.check_collision_point_rec mouse_pos rect in
  let is_left_clicked = R.is_mouse_button_pressed R.MouseButton.Left in
  (is_hovered, is_left_clicked)
;;

let update_cells_state ~cells ~is_hovered ~is_left_clicked ~x ~y =
  if is_hovered && is_left_clicked then cells.(x).(y) <- not cells.(x).(y)
;;

let make_rect x y =
  R.Rectangle.create
    (float_of_int @@ (x * Cell.size))
    (float_of_int @@ (y * Cell.size))
    (float_of_int Cell.size)
    (float_of_int Cell.size)
;;

let get_alive_neigbours ~cells ~x ~y =
  let check x y = if cells.(x).(y) then 1 else 0 in
  if x = 0 || y = 0 || x = Cells.rows - 1 || y = Cells.cols - 1 then
    -1
  else
    check (x - 1) (y - 1) + check x (y - 1) + check (x + 1) (y - 1) +
    check (x - 1) (y    )                   + check (x + 1) (y    ) +
    check (x - 1) (y + 1) + check x (y + 1) + check (x + 1) (y + 1)
[@@ocamlformat "disable"]

let draw_frame ~cells mouse_pos =
  ()
  ; Cells.iter (fun x y ->
      let rect = make_rect x y in
      let (is_hovered, is_left_clicked) = check_player_state mouse_pos rect in
      let alive_neigbours = get_alive_neigbours ~cells ~x ~y in
      ()
      ; update_cells_state ~cells ~is_hovered ~is_left_clicked ~x ~y
      ; draw_cell rect ~is_hovered ~is_alive:cells.(x).(y) ~alive_neigbours ~x ~y)
;;

let update_next_cells_state ~cells =
  Cells.iter (fun x y ->
    let alive_neigbours = get_alive_neigbours ~cells ~x ~y in
    let is_alive = cells.(x).(y) in
    let will_live = alive_neigbours = 3 || (is_alive && alive_neigbours = 2) in
    Next_cells.state.(x).(y) <- will_live)
;;

let advance_cells ~cells =
  Cells.iter (fun x y -> cells.(x).(y) <- Next_cells.state.(x).(y))
;;

let () =
  let pause = ref true in
  let ref_cells = ref (Cells.init ()) in
  ()
  ; R.set_target_fps 60
  ; R.init_window Window.w Window.h "Game of Life"
  ; while not (R.window_should_close ()) do
      let cells = !ref_cells in
      ()
      ; R.begin_drawing ()
      ; draw_frame ~cells (R.get_mouse_position ())
      ; update_next_cells_state ~cells
      ; if not !pause then advance_cells ~cells
      ; if R.is_key_pressed R.Key.Space then pause := not !pause
      ; if R.is_key_pressed R.Key.R then ref_cells := Cells.init ()
      ; R.end_drawing ()
    done
  ; R.close_window ()
;;
