open Core

let grid =
  In_channel.read_lines "../lib/p8.txt"
  |> List.map ~f:(fun e -> String.to_list e |> List.map ~f:int_of_char)

let max_row grid = List.length grid
let max_col grid = List.length (List.nth_exn grid 0)

let get_tree grid (row, col) =
  let first = List.nth grid row in
  match first with None -> None | Some x -> List.nth x col

let outside_grid (curr_row, curr_col) (max_row, max_col) =
  if curr_row >= max_row || curr_col >= max_col || curr_row < 0 || curr_col < 0
  then None
  else Some (curr_row, curr_col)

let is_visible grid (row, col) =
  let rec loop (curr_row, curr_col) (row_dir, col_dir) =
    let next_coords = (curr_row + row_dir, curr_col + col_dir) in
    let next = get_tree grid (curr_row + row_dir, curr_col + col_dir) in
    let curr = get_tree grid (curr_row, curr_col) in
    match next with
    | None -> true
    | Some n ->
        if Option.value_exn curr > n then loop (row_dir, col_dir) next_coords
        else false
  in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
  List.map ~f:(loop (row, col)) [ up; down; left; right ]
  |> List.fold ~init:true ~f:( && )

let parse_row grid (row, col) end_col =
  let rec loop accum curr_col =
    if curr_col > end_col then accum
    else loop (is_visible grid (row, curr_col) :: accum) (curr_col + 1)
  in
  loop [] col

let parse_col grid (row, col) end_row =
  let rec loop accum curr_row =
    if curr_row > end_row then accum
    else loop (is_visible grid (curr_row, col) :: accum) (curr_row + 1)
  in
  loop [] row

let parse grid =
  let rec loop (start_row, start_col) (end_row, end_col) =
    if start_row > end_row || start_col > end_col then []
    else
      let top_left = (start_row, start_col) in
      let bottom_left = (end_row, start_col) in
      let top_right = (start_row, end_col) in
      let first = parse_row grid top_left end_col in
      let second = parse_col grid top_left end_row in
      let third = parse_row grid bottom_left end_col in
      let fourth = parse_col grid top_right end_row in
      first @ second @ third @ fourth
      @ loop (start_row + 1, start_col + 1) (end_row - 1, end_col - 1)
  in
  loop (0, 0) (max_row grid, max_col grid)

let x =
  parse grid
  |> List.fold ~init:0 ~f:(fun accum e ->
         match e with false -> accum | true -> accum + 1)

let () = printf "%d\n" x
