open Core

module Point = struct
  type t = int * int [@@deriving hash, equal, compare, sexp_of]
end

let max_row grid = List.length grid
let max_col grid = List.length (List.nth_exn grid 0)

let get_tree grid (row, col) =
  let first = List.nth grid row in
  match first with None -> None | Some x -> List.nth x col

(* let outside_grid (curr_row, curr_col) (max_row, max_col) =
   if curr_row >= max_row || curr_col >= max_col || curr_row < 0 || curr_col < 0
   then None
   else Some (curr_row, curr_col) *)

let is_visible grid (row, col) =
  let rec loop tree (curr_row, curr_col) (row_dir, col_dir) =
    let next_coords = (curr_row + row_dir, curr_col + col_dir) in
    let curr = get_tree grid next_coords in
    match curr with
    | None ->
        (* if Tuple2.equal ~eq1:( = ) ~eq2:( = ) (3, 3) (row, col) then (
           printf "Next Row: %d Next Col: %d\n" (curr_row + row_dir)
             (curr_col + col_dir);
           printf "\n"); *)
        true
    | Some n ->
        (* if Tuple2.equal ~eq1:( = ) ~eq2:( = ) (3, 3) (row, col) then (
           printf "Row: %d Col: %d\n" row col;
           printf "Tree Value: %d\n"
           @@ Option.value_exn (get_tree grid (row, col));
           printf "Next Row: %d Next Col: %d\n" (curr_row + row_dir)
             (curr_col + col_dir);
           printf "Next tree value: %d\n" n;
           printf "\n"); *)
        if Option.value_exn tree > n then
          loop tree next_coords (row_dir, col_dir)
        else false
  in
  let up = (-1, 0) in
  let down = (1, 0) in
  let left = (0, -1) in
  let right = (0, 1) in
  List.map
    ~f:(loop (get_tree grid (row, col)) (row, col))
    [ up; down; left; right ]
  |> List.fold ~init:false ~f:( || )

let parse_row grid set (row, col) end_col =
  let rec loop curr_col =
    (* printf "%d %d\n" row curr_col; *)
    if curr_col > end_col then []
    else if
      (* printf "%b\n" @@ is_visible grid (row, curr_col); *)
      Hash_set.mem set (row, curr_col)
    then loop (curr_col + 1)
    else (
      Hash_set.add set (row, curr_col);
      is_visible grid (row, curr_col) :: loop (curr_col + 1))
  in
  loop col

let parse_col grid set (row, col) end_row =
  let rec loop curr_row =
    if curr_row > end_row then []
    else if Hash_set.mem set (curr_row, col) then loop (curr_row + 1)
    else (
      Hash_set.add set (curr_row, col);
      is_visible grid (curr_row, col) :: loop (curr_row + 1))
  in
  loop row

let parse grid visited_set =
  let rec loop (start_row, start_col) (end_row, end_col) =
    if start_row > end_row || start_col > end_col then []
    else
      let top_left = (start_row, start_col) in
      let bottom_left = (end_row, start_col) in
      let top_right = (start_row, end_col) in

      printf "top_left Row: %d Col: %d\n" start_row start_col;
      printf "bottom_left Row: %d Col: %d\n" end_row start_col;
      printf "top_right Row: %d Col: %d\n" start_row end_col;
      printf "\n";
      let first = parse_row grid visited_set top_left end_col in
      (* Over counting of corners here need to have a set for this*)
      let second = parse_col grid visited_set top_left end_row in
      let third = parse_row grid visited_set bottom_left end_col in
      let fourth = parse_col grid visited_set top_right end_row in

      (* printf "first \n";
         List.iter ~f:(printf "%b ") first;
         printf "\n";
         printf "second \n";
         List.iter ~f:(printf "%b ") second;
         printf "third \n";
         printf "\n";
         List.iter ~f:(printf "%b ") third;
         printf "fourth \n";
         printf "\n";
         List.iter ~f:(printf "%b ") fourth;
         printf "\n"; *)
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
  (* List.iter ~f:(printf "%b \n") i; *)
  List.fold ~init:0
    ~f:(fun accum e -> match e with false -> accum | true -> accum + 1)
    i

let () = printf "%d\n" x
