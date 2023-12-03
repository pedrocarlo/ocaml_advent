open Core

let tuple_section lst =
  match lst with
  | x :: y :: _ -> Some (int_of_string x, int_of_string y)
  | _ -> None

let split_line line =
  String.split ~on:',' line
  |> List.map ~f:(String.split ~on:'-')
  |> List.filter_map ~f:tuple_section

(* s1 contains s2 or vice-versa *)
let section_contains s =
  match s with
  | (a, b) :: (x, y) :: _ ->
      if a <= x && x <= b then true
      else if a <= y && y <= b then true
      else if x <= a && a <= y then true
      else if x <= b && b <= y then true
      else false
  | _ -> false

let times =
  In_channel.read_lines "../lib/p4.txt"
  |> List.map ~f:split_line
  |> List.map ~f:section_contains
  |> List.count ~f:Fun.id

let () = printf "%d\n" times
