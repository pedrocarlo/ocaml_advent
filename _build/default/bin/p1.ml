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

let rec max_elf_pouch curr_max ic =
  let curr = read_elf_pouch ic in
  match curr with
  | None -> curr_max
  | Some x ->
      if x > curr_max then max_elf_pouch x ic else max_elf_pouch curr_max ic

let m = max_elf_pouch 0 ic;;

print_int m;
print_newline ()
