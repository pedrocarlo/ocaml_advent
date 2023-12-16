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

let linear_move (tail_row, tail_col) (head_row, head_col) =
  let dirs = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
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

let move_tail (tail_row, tail_col) (head_row, head_col) =
  let tail = (tail_row, tail_col) in
  let head = (head_row, head_col) in
  if adjacent tail head then tail
  else if two_away tail head then linear_move tail head
  else if diagonal_away tail head then
    let () = printf "Head Row: %d Head Col: %d\n" head_row head_col in
    let () = printf "Tail Row: %d Tail Col: %d\n" tail_row tail_col in
    diagonal_move tail head
  else failwith "head too far away"

let eval_command set c knot_lst (head_row, head_col) =
  let (row_dir, col_dir), count =
    match c with
    | Up x -> ((1, 0), x)
    | Down x -> ((-1, 0), x)
    | Right x -> ((0, 1), x)
    | Left x -> ((0, -1), x)
  in
  let rec loop count knot_lst (curr_head_row, curr_head_col) =
    if Int.equal count 0 then (knot_lst, (curr_head_row, curr_head_col))
    else
      let next_head_row, next_head_col =
        add_point (row_dir, col_dir) (curr_head_row, curr_head_col)
      in
      let next_head_point = (next_head_row, next_head_col) in
      let rec knot_loop knot_lst curr_head accum =
        match knot_lst with
        | [] ->
            Hash_set.add set curr_head;
            (* only add last tail point *) accum
        | (curr_tail_row, curr_tail_col) :: tl ->
            let curr_tail_point = (curr_tail_row, curr_tail_col) in
            let next_tail_point = move_tail curr_tail_point curr_head in
            knot_loop tl next_tail_point (next_tail_point :: accum)
      in
      loop (count - 1)
        (List.rev (knot_loop knot_lst next_head_point []))
        next_head_point
  in
  loop count knot_lst (head_row, head_col)

let rec empty_knots_lst amount =
  match amount with 0 -> [] | _ -> (0, 0) :: (empty_knots_lst @@ (amount - 1))

let parse set commands knot_amount =
  List.fold commands
    ~init:(empty_knots_lst knot_amount, (0, 0))
    ~f:(fun (knot_lst, (head_row, head_col)) c ->
      eval_command set c knot_lst (head_row, head_col))

let set = Hash_set.create (module Point)

let commands =
  In_channel.read_lines "../lib/p9.txt" |> List.map ~f:parse_command

let _ = parse set commands 9

let () =
  Hash_set.to_list set
  |> List.sort ~compare:Point.compare
  |> List.iter ~f:(fun (row, col) -> printf "Row: %d Col: %d\n" row col)

let () = printf "%d\n" @@ Hash_set.length set
