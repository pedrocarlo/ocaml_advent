open Core

let item_priority c =
  let offset base c = Char.to_int c - Char.to_int base in
  match c with
  | 'a' .. 'z' -> 1 + offset 'a' c
  | 'A' .. 'Z' -> 27 + offset 'A' c
  | _ -> 0

(* MUCH Smarter than what I did *)
let find_duplicate first second third =
  Set.inter
    (Set.inter (Char.Set.of_list first) (Char.Set.of_list second))
    (Char.Set.of_list third)
  |> Set.choose

let custom_filter_map lines ~f =
  let rec loop lines accum =
    match lines with
    | x :: y :: z :: tl -> (
        match f x y z with
        | Some x -> loop tl (x :: accum)
        | None -> loop tl accum)
    | [] -> accum
    | _ -> accum
  in
  loop lines []

let s =
  In_channel.read_lines "../lib/p3.txt"
  |> List.map ~f:String.to_list
  |> custom_filter_map ~f:find_duplicate
  |> List.sum (module Int) ~f:item_priority

let () = printf "%d\n" s
