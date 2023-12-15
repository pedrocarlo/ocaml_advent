open Core

module Point = struct
  type t = int * int [@@deriving hash, equal, compare, sexp_of]
end

let max_row grid = List.length grid
let max_col grid = List.length (List.nth_exn grid 0)

let get_tree grid (row, col) =
  let first = List.nth grid row in
  match first with None -> None | Some x -> List.nth x col

let scenic_score grid (row, col) =
  let rec loop tree (curr_row, curr_col) (row_dir, col_dir) =
    let next_coords = (curr_row + row_dir, curr_col + col_dir) in
    let curr = get_tree grid next_coords in
    match curr with
    | None -> 0
    | Some n ->
        if Option.value_exn tree > n then
          1 + loop tree next_coords (row_dir, col_dir)
        else 1
  in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
  let x =
    List.map
      ~f:(loop (get_tree grid (row, col)) (row, col))
      [ up; down; left; right ]
    |> List.fold ~init:1 ~f:( * )
  in
  x

let parse_row grid set (row, col) end_col =
  let rec loop curr_col =
    if curr_col > end_col then []
    else if Hash_set.mem set (row, curr_col) then loop (curr_col + 1)
    else (
      Hash_set.add set (row, curr_col);
      scenic_score grid (row, curr_col) :: loop (curr_col + 1))
  in
  loop col

let parse_col grid set (row, col) end_row =
  let rec loop curr_row =
    if curr_row > end_row then []
    else if Hash_set.mem set (curr_row, col) then loop (curr_row + 1)
    else (
      Hash_set.add set (curr_row, col);
      scenic_score grid (curr_row, col) :: loop (curr_row + 1))
  in
  loop row

let parse grid visited_set =
  let rec loop (start_row, start_col) (end_row, end_col) =
    if start_row > end_row || start_col > end_col then []
    else
      let top_left = (start_row, start_col) in
      let bottom_left = (end_row, start_col) in
      let top_right = (start_row, end_col) in
      let first = parse_row grid visited_set top_left end_col in
      (* Over counting of corners here need to have a set for this*)
      let second = parse_col grid visited_set top_left end_row in
      let third = parse_row grid visited_set bottom_left end_col in
      let fourth = parse_col grid visited_set top_right end_row in
      first @ second @ third @ fourth
      @ loop (start_row + 1, start_col + 1) (end_row - 1, end_col - 1)
  in
  loop (0, 0) (max_row grid - 1, max_col grid - 1)

let grid =
  In_channel.read_lines "../lib/p8.txt"
  |> List.map ~f:(fun e ->
         String.to_list e |> List.map ~f:(fun e -> Char.to_int e - 48))

let visited = Hash_set.create (module Point)

let x =
  let i = parse grid visited in
  List.fold ~init:0 ~f:(fun accum e -> if e > accum then e else accum) i

let () = printf "%d\n" x
