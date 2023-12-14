open Core

let puzzle =
  String.strip @@ In_channel.read_all "../lib/p6.txt" |> String.to_list

let rec find_marker input chars count =
  if phys_equal (List.length chars) 14 then count
  else
    match input with
    | [] -> count
    | hd :: tl ->
        (* if phys_equal count 3369 then (
             print_string "Chars: ";
             List.rev chars |> List.iter ~f:(printf "%c ");
             printf " Count: %d\n" count;
             printf "   Current char: %c\n" hd);

           print_string "Chars: ";
           List.rev chars |> List.iter ~f:(printf "%c ");
           printf "   Current char: %c\n" hd;
           printf " Count: %d\n" count; *)
        if Set.mem (Char.Set.of_list chars) hd then
          find_marker tl
            ([ hd ]
            @ List.take_while ~f:(fun elem -> not (phys_equal hd elem)) chars)
            (count + 1)
        else find_marker tl (hd :: chars) (count + 1)

let count = find_marker puzzle [] 0
let () = printf "%d\n" count
