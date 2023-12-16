open Core

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

(* Operation can be just an int option *)

let parse_operation line =
  let split = String.split ~on:' ' line in
  match split with _ :: amount :: _ -> Some (int_of_string amount) | _ -> None

let rec cycle register_x operations prev_op count accum =
  let curr_accum =
    if modulo (count - 20) 40 = 0 && count < 221 then (
      printf "Count %d\n" count;
      printf "Register_x: %d\n" register_x;
      (count * register_x) :: accum)
    else accum
  in
  match prev_op with
  | None -> (
      match operations with
      | [] -> accum
      | hd :: tl -> (
          match hd with
          | None -> cycle register_x tl None (count + 1) curr_accum
          | add -> cycle register_x tl add (count + 1) curr_accum))
  | Some x -> cycle (register_x + x) operations None (count + 1) curr_accum

let operations =
  In_channel.read_lines "../lib/p10.txt" |> List.map ~f:parse_operation

let signals = cycle 1 operations None 1 []
let sum = List.fold ~init:0 ~f:( + ) signals
let () = printf "%d\n" sum
