open Core

let ic = In_channel.create "../lib/p5.txt"
(* let parse_stack = In_channel.input_line ic *)

let rec read_until_empty_line ic accum =
  let line = In_channel.input_line ic in
  match line with
  | Some "" -> accum
  | None -> accum
  | Some x -> read_until_empty_line ic (x :: accum)

(* let rec print_list lst =
   match lst with
   | [] -> ()
   | hd :: tl ->
       printf "%s\n" hd;
       print_list tl *)

let parse_stack_line line =
  let split = String.split line ~on:' ' in
  let rec loop lst accum space_count =
    match lst with
    | [] -> accum
    | hd :: tl -> (
        match hd with
        | "" ->
            if phys_equal space_count 3 then loop tl (Some " " :: accum) 0
              (* loop tl (None :: accum) true *)
            else loop tl (None :: accum) (space_count + 1)
        (* | "" -> loop tl (None :: accum) true *)
        | _ ->
            let formatted =
              if phys_equal (String.length hd) 3 then
                Some (String.drop_suffix (String.drop_prefix hd 1) 1)
              else Some hd
            in
            loop tl (formatted :: accum) 0)
  in
  loop split [] 0

let push_to_stack table id box =
  let stack = Hashtbl.find table id in
  match stack with Some x -> Stack.push x box | None -> ()

let rec push_multi table id boxes =
  let stack = Hashtbl.find table id in
  match boxes with
  | [] -> ()
  | hd :: tl ->
      (match stack with Some x -> Stack.push x hd | None -> ());
      push_multi table id tl

(* let pop_from_stack table id =
   let stack = Hashtbl.find table id in
   match stack with Some x -> Stack.pop x | None -> None *)

let pop_multi table id amount =
  let stack = Hashtbl.find table id in
  let rec loop accum counter =
    match counter with
    | 0 -> accum
    | _ -> (
        match stack with
        | Some x -> loop (Option.value_exn (Stack.pop x) :: accum) (counter - 1)
        | None -> accum)
  in
  loop [] amount

let id_and_stacks = read_until_empty_line ic []
let id_stacks = parse_stack_line @@ List.nth_exn id_and_stacks 0
(* let test = parse_stack_line @@ List.nth_exn id_and_stacks 8 *)

(* let () =
   List.filter_opt id_stacks |> List.iter ~f:(printf " %s ");
   print_endline "" *)

(* let () =
   let filtered = List.filter_opt test in
   let () = List.iter ~f:(printf " %s ") filtered in
   print_endline "";
   printf "%d\n" @@ List.length filtered *)

(* let stacks =
   List.map ~f:parse_stack_line
   @@ List.slice id_and_stacks 1 (List.length id_and_stacks - 1) *)

(* Filters None from stack which are the spaces between stacks *)
let parsed_stacks =
  let stacks =
    List.map ~f:parse_stack_line
    @@ List.slice id_and_stacks 1 (List.length id_and_stacks)
  in
  let rec loop lst accum =
    match lst with
    | [] -> accum
    | hd :: tl ->
        let filtered = List.filter_opt hd :: accum in
        (* List.iter ~f:(printf " %s ") (List.filter_opt hd);
           print_endline ""; *)
        loop tl filtered
  in
  loop stacks []

let table = Hashtbl.create (module String)

let create_stack_table table num =
  let tmp = Hashtbl.add table ~key:num ~data:(Stack.create ()) in
  match tmp with _ -> ()

let rec add_boxes_to_stacks table parsed_stacks =
  let rec inner_loop line_stack count =
    match line_stack with
    | [] -> ()
    | hd :: tl ->
        if not @@ phys_equal hd " " then
          push_to_stack table (string_of_int count) hd;
        inner_loop tl (count - 1)
  in
  match parsed_stacks with
  | [] -> ()
  | hd :: tl ->
      inner_loop hd 9;
      add_boxes_to_stacks table tl

(* Creating the initial stacks *)
let () = List.filter_opt id_stacks |> List.iter ~f:(create_stack_table table)

(* let () =
   List.iter
     ~f:(fun lst ->
       List.iter lst ~f:(printf " %s ");
       print_endline "")
     parsed_stacks *)

(* let () =
   List.iter ~f:(printf "%s\n")
   @@ List.slice id_and_stacks 1 (List.length id_and_stacks - 1) *)

(* Adds crates to stacks *)
let () = add_boxes_to_stacks table parsed_stacks
(* let () = Hashtbl.iteri table ~f:print_stack *)

let parse_command line =
  let split_command =
    String.split ~on:' ' line |> List.filter_map ~f:int_of_string_opt
  in
  match split_command with
  | x :: y :: z :: _ -> Some (x, string_of_int y, string_of_int z)
  | _ -> None

let eval_command table tuple =
  match tuple with
  | amount, src, dst ->
      let popped = pop_multi table src amount in
      push_multi table dst popped

let commands = In_channel.input_lines ic
let parsed_commands = List.filter_map ~f:parse_command commands

let print_stack ~key ~data =
  print_string (key ^ " ");
  let () = Stack.iter data ~f:(printf " %s ") in
  Out_channel.newline stdout

let peek_stacks table =
  let rec loop count =
    let stack = Hashtbl.find table (string_of_int count) in
    print_stack ~key:(string_of_int count) ~data:(Option.value_exn stack);
    loop (count + 1)
    (* match stack with
       | Some x ->
           printf "%s" @@ Stack.pop_exn x;
           loop (count + 1)
       | None -> () *)
  in
  loop 1

let () = List.iter ~f:(eval_command table) parsed_commands
let () = peek_stacks table

(* let () = print_list @@ id_and_stacks *)
