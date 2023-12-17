open Core

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

type monkey = {
  mutable items : int list;
  operation : int -> int;
  test : int -> int;
  test_num : int;
}

type operationToken = Var | Op of (int -> int -> int) | Constant of int

let token_operation token =
  match token with
  | "old" -> Var
  | "*" -> Op ( * )
  | "+" -> Op ( + )
  | _ -> Constant (int_of_string token)

let parse_operation operation =
  let token_lst =
    String.split ~on:' ' operation |> List.map ~f:token_operation
  in
  match token_lst with
  | v1 :: op :: v2 :: _ ->
      fun e ->
        let first =
          match v1 with
          | Var -> e
          | Constant x -> x
          | _ -> failwith "not a var or constant"
        in
        let second =
          match op with Op f -> f | _ -> failwith "not an operation symbol"
        in
        let third =
          match v2 with
          | Var -> e
          | Constant x -> x
          | _ -> failwith "not a var or constant"
        in
        second first third
  | _ -> failwith "not an operation"

let parse_items items =
  String.split items ~on:','
  |> List.map ~f:(fun e -> int_of_string (String.strip e))

let parse_test (test_val, t_branch, f_branch) e =
  let remainder = modulo e (int_of_string test_val) in
  match remainder with
  | 0 -> int_of_string t_branch
  | _ -> int_of_string f_branch

let parse_monkey line_block =
  (* printf "%s\n" line_block; *)
  let split =
    String.strip line_block |> String.split_lines |> List.map ~f:String.strip
  in
  match split with
  | _ :: start_items :: op :: test_case :: t_branch :: f_branch :: _ ->
      let prefixes =
        [
          "Starting items: ";
          "Operation: new = ";
          "Test: divisible by ";
          "If true: throw to monkey ";
          "If false: throw to monkey ";
        ]
      in
      let props =
        List.mapi
          ~f:(fun i e ->
            String.chop_prefix_exn e ~prefix:(List.nth_exn prefixes i))
          [ start_items; op; test_case; t_branch; f_branch ]
      in
      (* List.iter props ~f:(printf "%s\n"); *)
      let items = List.nth_exn props 0 |> parse_items in
      let operation = List.nth_exn props 1 |> parse_operation in
      let test, test_num =
        let slice = List.slice props 2 5 in
        match slice with
        | test_num :: t_branch :: f_branch :: _ ->
            (parse_test (test_num, t_branch, f_branch), int_of_string test_num)
        | _ -> failwith "not a test"
      in
      { items; operation; test; test_num }
  | _ -> failwith "not a monkey"

let add_item monkey ~item = monkey.items <- monkey.items @ [ item ]

let round monkeys rounds =
  (* Chinese remainder theorem good to know modulus arithmetic applies here *)
  let decrease_worry =
    List.fold monkeys ~init:1 ~f:(fun acc m -> acc * m.test_num)
  in
  let map = Hashtbl.create ~size:(List.length monkeys) (module Int) in
  List.iteri monkeys ~f:(fun i _ -> Hashtbl.set map ~key:i ~data:0);
  let increment_monkey i amount =
    let old = Hashtbl.find_exn map i in
    Hashtbl.set map ~key:i ~data:(old + amount)
  in
  let rec loop count =
    List.iteri monkeys ~f:(fun i m ->
        printf "Monkey: %d Items: " i;
        List.iter m.items ~f:(printf "%d ");
        printf "\n");
    printf "\n";
    if Int.equal count rounds then ()
    else
      let monkey_round i m =
        increment_monkey i (List.length m.items);
        List.iter m.items ~f:(fun item ->
            let new_item = m.operation item mod decrease_worry in
            m.test new_item |> List.nth_exn monkeys |> add_item ~item:new_item);
        m.items <- []
      in
      List.iteri monkeys ~f:monkey_round;
      loop (count + 1)
  in
  loop 0;
  Hashtbl.to_alist map
  |> List.sort ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)
  |> List.rev

let rec read_until_empty_line ic accum =
  let line = In_channel.input_line ~fix_win_eol:true ic in
  match line with
  | Some "" -> List.rev accum
  | None -> List.rev accum
  | Some x -> read_until_empty_line ic (x :: accum)

let rec line_block_lst ic =
  let result = read_until_empty_line ic [] in
  match result with
  | [] -> []
  | _ ->
      List.fold ~init:"" ~f:(fun init e -> init ^ e ^ "\n") result
      :: line_block_lst ic

(* let lines = In_channel.read_lines "../lib/p11.txt" *)
let ic = In_channel.create "../lib/p11.txt"
let monkeys = line_block_lst ic |> List.map ~f:parse_monkey

let monkey_business =
  let sorted_monkeys = round monkeys 10000 in
  List.iter sorted_monkeys ~f:(fun (k, d) ->
      printf "Monkey: %d Inspected Times: %d\n" k d);
  List.slice sorted_monkeys 0 2
  |> List.fold ~init:1 ~f:(fun accum (_, d) -> accum * d)

let () = printf "%d\n" monkey_business
