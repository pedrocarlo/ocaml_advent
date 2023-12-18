open Core
open Str

module Point = struct
  type t = int * int [@@deriving hash, equal, compare, sexp_of]
end

let print_point (row, col) = printf "Row: %d Col: %d\n" row col
let parse_row line = Str.split (regexp "") line |> Array.of_list

let grid lines =
  let rec loop lines =
    match lines with [] -> [] | hd :: tl -> parse_row hd :: loop tl
  in
  Array.of_list (loop lines)

(* Find S and E. Doing O(N^2) but just doing it once.
   Do not know yet how to short circuit here when I find both coords *)
let search_start_goal grid =
  let max_row = Array.length grid in
  let max_col = Array.length grid.(0) in
  let rec loop curr_row s_coords e_coords =
    if Int.equal curr_row max_row then (s_coords, e_coords)
    else
      let rec inner_loop curr_col s_coords e_coords =
        if Int.equal curr_col max_col then loop (curr_row + 1) s_coords e_coords
        else
          match grid.(curr_row).(curr_col) with
          | "S" ->
              grid.(curr_row).(curr_col) <- "a";
              inner_loop (curr_col + 1) (curr_row, curr_col) e_coords
          | "E" ->
              grid.(curr_row).(curr_col) <- "z";
              inner_loop (curr_col + 1) s_coords (curr_row, curr_col)
          | _ -> inner_loop (curr_col + 1) s_coords e_coords
      in
      inner_loop 0 s_coords e_coords
  in
  loop 0 (0, 0) (0, 0)

let add_coords (row, col) (other_row, other_col) =
  (row + other_row, col + other_col)

let equal_coords (row, col) (other_row, other_col) =
  Int.equal row other_row && Int.equal col other_col

let valid_coordinate grid (row, col) =
  let max_row = Array.length grid in
  let max_col = Array.length grid.(0) in
  if row < 0 || row >= max_row || col < 0 || col >= max_col then None
  else Some (row, col)

let valid_elevation grid (row, col) (other_row, other_col) =
  if grid.(row).(col) - grid.(other_row).(other_col) <= 1 then
    Some (other_row, other_col)
  else None

let valid_neighbors grid (row, col) =
  List.map
    ~f:(fun e -> add_coords (row, col) e)
    [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
  |> List.filter_map ~f:(fun e -> valid_coordinate grid e)
  |> List.filter_map ~f:(fun e -> valid_elevation grid (row, col) e)

let bfs grid start_coords (end_row, end_col) =
  let parent_map = Hashtbl.create (module Point) in
  let dist_map = Hashtbl.create (module Point) in
  Hashtbl.set dist_map ~key:start_coords ~data:0;
  (* Got the the way to do a prioqueue from
     https://github.com/nathanfarlow/aoc-2022-ocaml/blob/main/day12/main.ml *)
  let queue =
    Pairing_heap.create
      ~cmp:(fun (i1, j1) (i2, j2) ->
        let x = Hashtbl.find dist_map (i1, j1) in
        let y = Hashtbl.find dist_map (i2, j2) in
        match (x, y) with
        | None, None -> Int.compare Int.max_value Int.max_value
        | Some i, None -> Int.compare i Int.max_value
        | None, Some j -> Int.compare j Int.max_value
        | Some i, Some j -> Int.compare i j)
      ()
  in
  Pairing_heap.add queue start_coords;
  let rec loop () =
    if Pairing_heap.is_empty queue then ()
    else
      let curr = Pairing_heap.pop_exn queue in
      if equal_coords curr (end_row, end_col) then ()
      else
        let neighbors = valid_neighbors grid curr in
        print_point curr;
        let curr_dst =
          match Hashtbl.find dist_map curr with
          | None -> Int.max_value
          | Some x -> x
        in
        List.iter neighbors ~f:(fun e ->
            let nei_dist = curr_dst + 1 in
            if
              Int.( < ) nei_dist
              @@
              match Hashtbl.find dist_map e with
              | None -> Int.max_value
              | Some x -> x
            then (
              (* printf "%d\n" nei_dist; *)
              Hashtbl.set dist_map ~key:e ~data:nei_dist;
              Hashtbl.set parent_map ~key:e ~data:curr;
              Pairing_heap.add queue e));
        loop ()
  in
  loop ();
  Hashtbl.find_exn dist_map (end_row, end_col)

let lines = In_channel.read_lines "../lib/p12.txt"
let tmp = grid lines
let (start_row, start_col), (end_row, end_col) = search_start_goal tmp

let config =
  tmp
  |> Array.map
       ~f:(Array.map ~f:(fun e -> Char.of_string e |> int_of_char |> ( - ) 48))

let x = bfs config (start_row, start_col) (end_row, end_col)
let () = printf "%d\n" x
