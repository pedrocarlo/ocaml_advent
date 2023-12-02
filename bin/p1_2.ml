let ic = open_in "../lib/p1.txt"
let parse_line line = match line with "" -> 0 | _ -> int_of_string line

let rec read_elf_pouch ic =
  try
    let line = parse_line (input_line ic) in
    match line with
    | 0 -> Some 0
    | _ -> (
        match read_elf_pouch ic with Some x -> Some (x + line) | None -> None)
  with _ ->
    close_in_noerr ic;
    None

let rec filter l x =
  match l with
  | [] -> []
  | h :: t -> if x > h then x :: filter t h else h :: filter t x

let rec max_elf_pouchs curr_maxs ic =
  let curr = read_elf_pouch ic in
  match curr with
  | None -> curr_maxs
  | Some x -> max_elf_pouchs (filter curr_maxs x) ic

let m = max_elf_pouchs [ 0; 0; 0 ] ic
let s = List.fold_right ( + ) m 0;;

print_int s;
print_newline ()
