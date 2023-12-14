open Core

let puzzle =
  String.strip @@ In_channel.read_all "../lib/p6.txt" |> String.to_list

let rec find_marker input chars =
  if phys_equal (List.length chars) 4 then -1
    (* Not elegant over counting by one at this stage*)
  else
    match input with
    | [] -> assert false
    | hd :: tl ->
        (* print_string "Chars: ";
           List.iter ~f:(printf "%c ") chars;
           printf "   Current char: %c\n" hd; *)
        (* printf " Count: %d\n" count; *)
        if Set.mem (Char.Set.of_list chars) hd then find_marker tl [] + 1
        else find_marker tl (hd :: chars) + 1

let count = find_marker puzzle []
let () = printf "%d\n" count
