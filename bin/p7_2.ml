open Core
open Base

type file_meta = { size : int; name : string }

type file_type =
  | Dir of (file_type * string * file_type list ref)
  | File of file_meta
(* | None *)

type command = Ls | Cd of string

let rec root = Dir (root, "/", ref [])

let get_parent_dir dir =
  match dir with Dir (parent, _, _) -> parent | _ -> failwith "not a dir"

let get_dir_files dir =
  match dir with Dir (_, _, files) -> files | _ -> failwith "not a dir"

let find_dir dir dir_name =
  match dir with
  | Dir (_, _, files) ->
      Option.value_exn (* Using this Option as could not find List.find_exn*)
        (List.find !files ~f:(fun e ->
             match e with
             | Dir (_, name, _) -> String.equal name dir_name
             | _ -> false))
  | _ -> failwith "not a dir"

let is_command line = String.is_prefix line ~prefix:"$"

(* already stripped from "$" *)
let parse_command line =
  let args = String.split line ~on:' ' in
  match args with
  | [] -> failwith "not a command"
  | _ :: dir :: _ -> Cd dir
  | _ :: _ -> Ls

let eval_command (parent, curr_dir) command =
  match command with
  | Ls -> (parent, curr_dir)
  | Cd x -> (
      match x with
      | ".." -> (get_parent_dir parent, parent)
      | "/" -> (root, root)
      | _ -> (curr_dir, find_dir curr_dir x))

let parse_file curr_dir line =
  let args = String.strip line |> String.split ~on:' ' in
  match args with
  | first :: name :: _ ->
      if String.equal first "dir" then Dir (curr_dir, name, ref [])
      else File { size = Int.of_string first; name }
  | _ -> failwith "not a file"

let parse_dir dir file =
  match dir with
  | File _ -> failwith "cannot have dir as a file"
  | Dir (_, _, file_lst) ->
      if not (List.mem !file_lst file ~equal:phys_equal) then
        file_lst := file :: !file_lst

let parse ~root lines =
  let rec loop (parent, curr_dir) lines =
    match lines with
    | [] -> ()
    | line :: tl ->
        if is_command line then
          let c =
            eval_command (parent, curr_dir)
              (parse_command @@ String.chop_prefix_if_exists ~prefix:"$ " line)
          in
          loop c tl
        else
          let file = parse_file curr_dir line in
          parse_dir curr_dir file;
          loop (parent, curr_dir) tl
  in
  loop (root, root) lines

let rec print_spaces x =
  if Int.equal x 0 then ()
  else (
    printf "    ";
    print_spaces (x - 1))

let print_filesystem root =
  let rec loop curr_dir level =
    let helper file =
      match file with
      | File x ->
          print_spaces @@ (level + 1);
          printf "- %s (file, size=%d)\n" x.name x.size
      | Dir (_, _, _) -> loop file @@ (level + 1)
      (* print_spaces level;
         printf "- %s (dir)\n" name *)
    in
    match curr_dir with
    | File _ -> failwith "should be a dir"
    | Dir (_, name, _) ->
        print_spaces level;
        printf "- %s (dir)\n" name;
        List.iter ~f:helper !(get_dir_files curr_dir)
  in
  loop root 0

let size_dir dir =
  let rec loop curr_dir =
    let helper curr_size file =
      match file with
      | File x -> curr_size + x.size
      | Dir (_, _, _) -> curr_size + loop file
    in
    match curr_dir with
    | File _ -> failwith "should be a dir"
    | Dir (_, _, _) -> List.fold ~init:0 ~f:helper !(get_dir_files curr_dir)
  in
  loop dir

let is_dir file = match file with File _ -> false | Dir _ -> true

let get_dirs dir =
  let rec loop accum files =
    match files with
    | [] -> accum
    | hd :: tl -> if is_dir hd then loop (hd :: accum) tl else loop accum tl
  in
  loop [] !(get_dir_files dir)

let dir_with_size_at_most root max_size =
  let rec loop curr_dir =
    match curr_dir with
    | File _ -> failwith "can only be dir"
    | Dir (_, _, _) ->
        let curr_size = size_dir curr_dir in
        (* printf "%d\n" curr_size; *)
        if curr_size > max_size || phys_equal curr_size 0 then
          List.fold ~init:[]
            ~f:(fun accum file ->
              match file with Dir _ -> accum @ loop file | _ -> accum)
            (get_dirs curr_dir)
        else
          List.fold ~init:[ curr_size ]
            ~f:(fun accum file ->
              match file with Dir _ -> accum @ loop file | _ -> accum)
            (get_dirs curr_dir)
  in
  loop root

let filesystem_size = 70000000
let update_size = 30000000
let () = In_channel.read_lines "../lib/p7.txt" |> parse ~root
let () = print_filesystem root
let curr_unused = filesystem_size - size_dir root

let x =
  dir_with_size_at_most root filesystem_size
  |> List.fold
       ~f:(fun curr_min curr ->
         if curr + curr_unused < update_size then curr_min
         else min curr_min curr)
       ~init:filesystem_size

(* let y = size_dir root *)
let () = printf "%d\n" x
