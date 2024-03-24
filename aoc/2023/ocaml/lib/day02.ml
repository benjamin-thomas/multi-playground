let example =
  {|

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

|}
;;

module GameParser = struct
  open Sexplib.Std

  type round =
    { red : int
    ; green : int
    ; blue : int
    }
  [@@deriving sexp]

  type game =
    { id : int
    ; rounds : round list
    }
  [@@deriving sexp]

  let int_or_zero str = Option.value ~default:0 @@ int_of_string_opt @@ str

  let update_round round = function
  | [ n_str; "red" ]   -> { round with   red = int_or_zero n_str }
  | [ n_str; "green" ] -> { round with green = int_or_zero n_str }
  | [ n_str; "blue" ]  -> { round with  blue = int_or_zero n_str }
  | _ -> round
[@@ocamlformat "disable"]

  let to_round str =
    let to_add =
      str
      |> String.split_on_char ','
      |> List.map (fun s -> s |> String.trim |> String.split_on_char ' ')
    in
    List.fold_left update_round { red = 0; green = 0; blue = 0 } to_add
  ;;

  let parse_rounds str = str |> String.split_on_char ';' |> List.map to_round

  let parse_game_id str =
    match str |> String.split_on_char ' ' with
    | [ _; id ] -> int_of_string_opt id
    | _ -> None
  ;;

  let parse_game str =
    match String.split_on_char ':' str with
    | [ game_id; rounds ] ->
      parse_game_id game_id |> Option.map (fun id -> { id; rounds = parse_rounds rounds })
    | _ -> None
  ;;

  let parse_games str = str |> String.split_on_char '\n' |> List.filter_map parse_game

  let%expect_test _ =
    let open Core in
    ()
    ; print_s [%sexp (parse_games example : game list)]
    ; [%expect
        {|
    (((id 1)
      (rounds
       (((red 4) (green 0) (blue 3)) ((red 1) (green 2) (blue 6))
        ((red 0) (green 2) (blue 0)))))
     ((id 2)
      (rounds
       (((red 0) (green 2) (blue 1)) ((red 1) (green 3) (blue 4))
        ((red 0) (green 1) (blue 1)))))
     ((id 3)
      (rounds
       (((red 20) (green 8) (blue 6)) ((red 4) (green 13) (blue 5))
        ((red 1) (green 5) (blue 0)))))
     ((id 4)
      (rounds
       (((red 3) (green 1) (blue 6)) ((red 6) (green 3) (blue 0))
        ((red 14) (green 3) (blue 15)))))
     ((id 5)
      (rounds (((red 6) (green 3) (blue 1)) ((red 1) (green 2) (blue 2)))))) |}]
  ;;
end

(*
   Game 1: 3 blue,  4 red
   ; 1 red,   2 green, 6 blue
   ; 2 green
   = possible

   Game 2: 1 blue,  2 green
   ; 3 green, 4 blue,  1 red
   ; 1 green, 1 blue
   = possible

   Game 3: 8 green, 6 blue, 20 red
   ; 5 blue, 4 red, 13 green
   ; 5 green, 1 red
   = impossible because 20 red

   Game 4: 1 green, 3 red, 6 blue
   ; 3 green, 6 red
   ; 3 green, 15 blue, 14 red
   = impossible because 15 blue cubes

   Game 5: 6 red, 1 blue, 3 green
   ; 2 blue, 1 red, 2 green
   = possible
*)

type set = GameParser.round
type game = GameParser.game

(** The maximum allowed number of cubes for a valid game *)
type remaining = GameParser.round [@@deriving sexp]

let rec _is_valid_game (r : remaining) (sets : set list) id =
  match sets with
  | [] -> r.red >= 0 && r.green >= 0 && r.blue >= 0
  | x :: xs ->
    r.red >= 0
    && r.green >= 0
    && r.blue >= 0
    && _is_valid_game
         { red = r.red - x.red; green = r.green - x.green; blue = r.blue - x.blue }
         xs
         id
;;

let is_valid_game (rule : remaining) (sets : set list) _id =
  let chk (set : set) =
    set.red <= rule.red && set.green <= rule.green && set.blue <= rule.blue
  in
  match sets with
  | [] -> false
  | x :: xs -> List.fold_left (fun was_valid set -> was_valid && chk set) (chk x) xs
;;

let%test _ = not @@ is_valid_game { red = 0; green = 0; blue = 0 } [] 0

let%test _ =
  not
  @@ is_valid_game { red = 0; green = 0; blue = 0 } [ { red = 1; green = 0; blue = 0 } ] 0
;;

let%test _ =
  is_valid_game { red = 1; green = 0; blue = 0 } [ { red = 1; green = 0; blue = 0 } ] 0
;;

let%test _ =
  not
  @@ is_valid_game { red = 1; green = 0; blue = 0 } [ { red = 2; green = 0; blue = 0 } ] 0
;;

let valid_game_indexes (rule : remaining) (games : game list) =
  games
  |> List.filter_map (fun (game : game) ->
    if is_valid_game rule game.rounds game.id then
      Some game.id
    else
      None)
;;

let part1_rule : remaining = { red = 12; green = 13; blue = 14 }

module TestGames = struct
  let game1 : game =
    { id = 1
    ; rounds =
        [ { red = 3; green = 4; blue = 0 }
        ; { red = 1; green = 2; blue = 6 }
        ; { red = 2; green = 0; blue = 0 }
        ]
    }
  ;;

  let game2 : game =
    { id = 2
    ; rounds =
        [ { red = 1; green = 2; blue = 0 }
        ; { red = 3; green = 4; blue = 1 }
        ; { red = 1; green = 1; blue = 0 }
        ]
    }
  ;;

  let game3 : game =
    { id = 3
    ; rounds =
        [ { red = 8; green = 6; blue = 20 }
        ; { red = 5; green = 4; blue = 13 }
        ; { red = 5; green = 1; blue = 0 }
        ]
    }
  ;;

  let game4 : game =
    { id = 4
    ; rounds =
        [ { red = 1; green = 3; blue = 6 }
        ; { red = 3; green = 6; blue = 0 }
        ; { red = 3; green = 15; blue = 14 }
        ]
    }
  ;;

  let solve_part1 games = games |> valid_game_indexes part1_rule |> List.fold_left ( + ) 0

  let game5 : game =
    { id = 5
    ; rounds = [ { red = 6; green = 1; blue = 3 }; { red = 2; green = 1; blue = 2 } ]
    }
  ;;

  let%test "game 1" = is_valid_game part1_rule game1.rounds game1.id
  let%test "game 2" = is_valid_game part1_rule game2.rounds game2.id
  let%test "game 3" = not @@ is_valid_game part1_rule game3.rounds game3.id
  let%test "game 4" = not @@ is_valid_game part1_rule game4.rounds game4.id
  let%test "game 5" = is_valid_game part1_rule game5.rounds game5.id

  let%expect_test _ =
    let open Core in
    ()
    ; print_s
        [%sexp
          (valid_game_indexes part1_rule [ game1; game2; game3; game4; game5 ] : int list)]
    ; [%expect {| (1 2 5) |}]
  ;;

  let games_input () =
    let path =
      "/home/benjamin/code/github.com/benjamin-thomas/multi-playground/aoc/2023/_inputs/day02.txt"
    in
    let content = In_channel.with_open_bin path In_channel.input_all in
    GameParser.parse_games content
  ;;

  let%test_unit "part 1" =
    let open Core in
    ()
    ; [%test_result: int] ~expect:8 @@ solve_part1 [ game1; game2; game3; game4; game5 ]
    ; [%test_result: int] ~expect:3 @@ solve_part1 [ game1; game2; game3; game4 ]
    ; [%test_result: int] ~expect:2679 @@ solve_part1 @@ games_input ()
  ;;
end

(*
   Parse a string such as:
   6 blue, 6 red, 2 green

   Into a set

   I may have;
*)
let parse_item s =
  try s |> String.split_on_char ' ' |> List.hd |> int_of_string with
  | _ -> 0
;;

let parse_set s : GameParser.round option =
  let rgb =
    s
    |> String.split_on_char ','
    |> List.map (fun s -> s |> String.trim |> String.split_on_char ' ' |> List.rev)
    |> List.sort compare
  in
  match rgb with
  | [ [ "blue"; blue ]; [ "green"; green ]; [ "red"; red ] ] ->
    Some
      { red = int_of_string red; green = int_of_string green; blue = int_of_string blue }
  | _ -> None
;;

(* === PART 2 === *)

let max_color (rounds : GameParser.round list) =
  List.fold_left
    (fun (acc : GameParser.round) (round : GameParser.round) ->
      let red = max acc.red round.red in
      let green = max acc.green round.green in
      let blue = max acc.blue round.blue in
      ({ red; green; blue } : GameParser.round))
    ({ red = 0; green = 0; blue = 0 } : GameParser.round)
    rounds
;;

let power (cubes : set list) =
  let m = cubes |> max_color in
  m.red * m.green * m.blue
;;

let powers (games : GameParser.game list) =
  games
  |> List.map (fun (game : GameParser.game) -> game.rounds |> power)
  |> List.fold_left ( + ) 0
;;

let games_input () =
  let path =
    "/home/benjamin/code/github.com/benjamin-thomas/multi-playground/aoc/2023/_inputs/day02.txt"
  in
  let content = In_channel.with_open_bin path In_channel.input_all in
  GameParser.parse_games content
;;

let%expect_test _ =
  let open Core in
  let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
  let game = GameParser.parse_game input |> Option.value_exn in
  let games = GameParser.parse_games example in
  ()
  ; print_s [%sexp (game : GameParser.game)]
  ; [%expect
      {|
    ((id 1)
     (rounds
      (((red 4) (green 0) (blue 3)) ((red 1) (green 2) (blue 6))
       ((red 0) (green 2) (blue 0))))) |}]
  ; ()
  ; print_s [%sexp (max_color game.rounds : GameParser.round)]
  ; [%expect {|
    ((red 4) (green 2) (blue 6)) |}]
  ; ()
  ; print_s [%sexp (power game.rounds : int)]
  ; [%expect {| 48 |}]
  ; ()
  ; print_s [%sexp (powers games : int)]
  ; [%expect {| 2286 |}]
  ; ()
  ; print_s [%sexp (powers @@ games_input () : int)]
  ; [%expect {| 77607 |}]
;;
