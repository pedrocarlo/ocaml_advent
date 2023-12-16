open Core

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

(* Operation can be just an int option *)

let parse_operation line =
  let split = String.split ~on:' ' line in
  match split with _ :: amount :: _ -> Some (int_of_string amount) | _ -> None

let draw cycle_count sprite_pos =
  if
    List.map
      ~f:(fun e -> Int.equal (e + sprite_pos) (modulo (cycle_count - 1) 40))
      [ -1; 0; 1 ]
    |> List.fold ~init:false ~f:( || )
  then printf "#"
  else printf "."

let rec cycle register_x operations prev_op count =
  if Int.equal (modulo (count - 1) 40) 0 then printf "\n";
  draw count register_x;
  match prev_op with
  | None -> (
      match operations with
      | [] -> ()
      | hd :: tl -> (
          match hd with
          | None -> cycle register_x tl None (count + 1)
          | add -> cycle register_x tl add (count + 1)))
  | Some x -> cycle (register_x + x) operations None (count + 1)

let operations =
  In_channel.read_lines "../lib/p10.txt" |> List.map ~f:parse_operation

let () = cycle 1 operations None 1
