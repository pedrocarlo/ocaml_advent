let ic = open_in "../lib/p1.txt" in
try
  let line = input_line ic in
  match line with " " -> close_in ic
with e ->
  close_in_noerr ic;
  raise e
