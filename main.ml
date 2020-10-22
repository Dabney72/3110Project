open Graphics

(** [create_grid (start_x, start_y) num_rows num_cols box_size] draws a grid
    centered at (start_x, start_y) with [num_rows] rows and [num_cols] columns.
    The size of each box is given by [box_size]. *)
let create_grid (start_x, start_y) num_rows num_cols box_size =
  let grid_width = num_cols * box_size in
  let grid_height = num_rows * box_size in
  for r = 0 to num_rows - 1 do
    for c = 0 to num_cols - 1 do
      draw_rect (start_x + c * box_size - grid_width / 2) (start_y + r * box_size - grid_height / 2) box_size box_size;
    done
  done

let start () =
  Graphics.open_graph "";
  let width = size_x () in
  let height = size_y () in
  create_grid (width / 2, height / 2) 20 10 20;
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then raise Exit
  done

let () = start ()