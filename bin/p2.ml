let ic = open_in "../lib/p1.txt"

let parse_line line =
  match line with "" -> None | _ -> Some (String.split_on_char ' ' line)




