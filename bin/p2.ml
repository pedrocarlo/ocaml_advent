open Core

type outcome = Loss | Tie | Win
type move = Rock | Paper | Scissors

let split_line line =
  match line with "" -> None | _ -> Some (String.split ~on:' ' line)

let move_value m = match m with Rock -> 1 | Paper -> 2 | Scissors -> 3

let move_pair round =
  match round with
  | Some [ opponent; me ] ->
      let player1 =
        match opponent with
        | "A" -> Some Rock
        | "B" -> Some Paper
        | "C" -> Some Scissors
        | _ -> None
      in
      let player2 =
        match me with
        | "X" -> Some Rock
        | "Y" -> Some Paper
        | "Z" -> Some Scissors
        | _ -> None
      in
      Option.both player1 player2
  | _ -> None

let parse_line line = split_line line |> move_pair
let outcome_value outcome = match outcome with Loss -> 0 | Tie -> 3 | Win -> 6

let find_outcome (opponent, me) =
  let round =
    match (opponent, me) with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> outcome_value Loss
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> outcome_value Tie
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> outcome_value Win
  in
  round + move_value me

let score =
  In_channel.read_lines "../lib/p2.txt"
  |> List.filter_map ~f:parse_line
  |> List.sum (module Int) ~f:find_outcome

let () = printf "Score: %d" score
