open Core

type command = Up of int | Down of int | Right of int | Left of int

module Point = struct
  type t = int * int [@@deriving hash, equal, compare, sexp_of]
end

let add_point (p1_r, p1_c) (p2_r, p2_c) = (p1_r + p2_r, p1_c + p2_c)

let parse_direction dir count =
  match dir with
  | "R" -> Right count
  | "L" -> Left count
  | "U" -> Up count
  | "D" -> Down count
  | _ -> failwith "not a direction"

let parse_command line =
  let s = String.split line ~on:' ' in
  match s with
  | dir :: count :: _ -> parse_direction dir @@ int_of_string count
  | _ -> failwith "not a command"

let adjacent (r1, c1) (r2, c2) =
  List.map
    ~f:(fun (r_dir, c_dir) ->
      Int.equal (r1 + r_dir) r2 && Int.equal (c1 + c_dir) c2)
    [
      (0, 0);
      (1, 0);
      (-1, 0);
      (0, 1);
      (0, -1);
      (1, 1);
      (1, -1);
      (-1, 1);
      (-1, -1);
    ]
  |> List.fold ~init:false ~f:( || )

let two_away (r1, c1) (r2, c2) =
  List.map
    ~f:(fun (r_dir, c_dir) ->
      Int.equal (r1 + r_dir) r2 && Int.equal (c1 + c_dir) c2)
    [ (2, 0); (-2, 0); (0, 2); (0, -2) ]
  |> List.fold ~init:false ~f:( || )

let diagonal_away (r1, c1) (r2, c2) =
  (not @@ Int.equal r1 r2) && (not @@ Int.equal c1 c2)

let diagonal_move (tail_row, tail_col) (head_row, head_col) =
  let dirs = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ] in
  let rec loop dirs min_dst min_tail =
    match dirs with
    | [] -> min_tail
    | dir :: tl ->
        let new_row, new_col = add_point (tail_row, tail_col) dir in
        printf "";
        let curr_dst =
          ((new_row - head_row) * (new_row - head_row))
          + ((new_col - head_col) * (new_col - head_col))
          (* Pythagoras without square root*)
        in
        if curr_dst < min_dst then loop tl curr_dst (new_row, new_col)
        else loop tl min_dst min_tail
  in
  loop dirs Int.max_value (tail_row, tail_col)

let move_tail dir (tail_row, tail_col) (head_row, head_col) =
  let tail = (tail_row, tail_col) in
  let head = (head_row, head_col) in
  if adjacent tail head then tail
  else if two_away tail head then add_point tail dir
  else if diagonal_away tail head then
    let () = printf "Head Row: %d Head Col: %d\n" head_row head_col in
    let () = printf "Tail Row: %d Tail Col: %d\n" tail_row tail_col in
    diagonal_move tail head
  else failwith "head too far away"

let eval_command set c (tail_row, tail_col) (head_row, head_col) =
  let (row_dir, col_dir), count =
    match c with
    | Up x -> ((1, 0), x)
    | Down x -> ((-1, 0), x)
    | Right x -> ((0, 1), x)
    | Left x -> ((0, -1), x)
  in
  let rec loop count (curr_tail_row, curr_tail_col)
      (curr_head_row, curr_head_col) =
    if Int.equal count 0 then
      ((curr_tail_row, curr_tail_col), (curr_head_row, curr_head_col))
    else
      let next_head_row, next_head_col =
        add_point (row_dir, col_dir) (curr_head_row, curr_head_col)
      in
      let next_head_point = (next_head_row, next_head_col) in
      let curr_tail_point = (curr_tail_row, curr_tail_col) in
      let next_tail_point =
        move_tail (row_dir, col_dir) curr_tail_point next_head_point
      in
      Hash_set.add set next_tail_point;
      let res = loop (count - 1) next_tail_point next_head_point in
      res
  in

  (* Hash_set.add set (tail_row, tail_col); *)
  loop count (tail_row, tail_col) (head_row, head_col)

let parse set commands =
  List.fold commands
    ~init:((0, 0), (0, 0))
    ~f:(fun ((tail_row, tail_col), (head_row, head_col)) c ->
      eval_command set c (tail_row, tail_col) (head_row, head_col))

let set = Hash_set.create (module Point)

let commands =
  In_channel.read_lines "../lib/p9.txt" |> List.map ~f:parse_command

let _ = parse set commands

let () =
  Hash_set.to_list set
  |> List.sort ~compare:Point.compare
  |> List.iter ~f:(fun (row, col) -> printf "Row: %d Col: %d\n" row col)

let () = printf "%d\n" @@ Hash_set.length set
