open Core

let split_compartment line =
  let string_list = String.to_list line in
  List.split_n string_list (String.length line / 2)

let item_priority c =
  let offset base c = Char.to_int c - Char.to_int base in
  match c with
  | 'a' .. 'z' -> 1 + offset 'a' c
  | 'A' .. 'Z' -> 27 + offset 'A' c
  | _ -> 0

(* let rec find_duplicate (set1, set2) (lst1, lst2) =
   match (lst1, lst2) with
   | x :: t, y :: j ->
       if Hash_set.mem set1 x then Some x
       else if Hash_set.mem set y then Some y
       else if Char.equal x y then Some x
       else (
         Hash_set.add set x;
         Hash_set.add set y;

         find_duplicate set (t, j))
   | _, _ -> None *)

(* MUCH Smarter than what I did *)
let find_duplicate (first, second) =
  Set.inter (Char.Set.of_list first) (Char.Set.of_list second) |> Set.choose

let parse_line line =
  let pair = split_compartment line in
  find_duplicate pair

let s =
  In_channel.read_lines "../lib/p3.txt"
  |> List.filter_map ~f:parse_line
  |> List.sum (module Int) ~f:item_priority

let () = printf "%d\n" s
