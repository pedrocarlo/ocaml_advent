open Core

type outcome = Loss | Tie | Win
type move = Rock | Paper | Scissors

let split_line line =
  match line with "" -> None | _ -> Some (String.split ~on:' ' line)

let move_value m = match m with Rock -> 1 | Paper -> 2 | Scissors -> 3

let move_pair round =
  match round with
  | Some [ opponent; outcome ] ->
      let player1 =
        match opponent with
        | "A" -> Some Rock
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | _ -> None
      in
      let out =
        match outcome with
        | "X" -> Some Loss
        | "Y" -> Some Tie
        | "Z" -> Some Win
        | _ -> None
      in
      Option.both player1 out
  | _ -> None

let parse_line line = split_line line |> move_pair
let outcome_value outcome = match outcome with Loss -> 0 | Tie -> 3 | Win -> 6

let find_outcome (opponent, out) =
  let round =
    match (opponent, out) with
    | Paper, Loss | Rock, Tie | Scissors, Win -> move_value Rock
    | Scissors, Loss | Paper, Tie | Rock, Win -> move_value Paper
    | Rock, Loss | Scissors, Tie | Paper, Win -> move_value Scissors
  in
  round + outcome_value out

let score =
  In_channel.read_lines "../lib/p2.txt"
  |> List.filter_map ~f:parse_line
  |> List.sum (module Int) ~f:find_outcome

let () = printf "Score: %d" score
