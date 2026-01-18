module Day01 exposing (..)

import Platform


type Rotation
    = Left Int
    | Right Int


parseLine : String -> Maybe Rotation
parseLine line =
    let
        ( first, rest ) =
            ( String.left 1 line
            , String.dropLeft 1 line
            )
    in
    case first of
        "L" ->
            Maybe.map
                Left
                (String.toInt rest)

        "R" ->
            Maybe.map
                Right
                (String.toInt rest)

        _ ->
            Nothing


step1 : Rotation -> ( Int, Int ) -> ( Int, Int )
step1 rotation ( count, dial ) =
    let
        result =
            modBy 100 <|
                case rotation of
                    Left x ->
                        dial - x

                    Right x ->
                        dial + x
    in
    ( if result == 0 then
        count + 1

      else
        count
    , result
    )


solvePart1 : String -> Int
solvePart1 input =
    input
        |> String.split "\n"
        |> List.map parseLine
        |> List.filterMap identity
        |> List.foldl step1 ( 0, 50 )
        |> Tuple.first


divBy : Int -> Int -> Int
divBy n x =
    x // n


step2 : Rotation -> ( Int, Int ) -> ( Int, Int )
step2 rotation ( count, dial ) =
    let
        raw =
            case rotation of
                Left x ->
                    dial - x

                Right x ->
                    dial + x

        newDial =
            modBy 100 raw

        turns =
            let
                clockwise =
                    case rotation of
                        Right _ ->
                            True

                        Left _ ->
                            False

                crossings =
                    {-
                       Handles negative integer div like in Python, Ruby, Haskell
                       Not like in C, Java, JavaScript, Elm, etc.

                       > floor ((toFloat -18) / 100)
                       -1 : Int

                       > floor ((toFloat 18) / 100)
                       0 : Int


                       [1] pry(main)> -18 / 100
                       => -1

                       [3] pry(main)> 18 / 100
                       => 0

                    -}
                    floor (toFloat raw / 100)
            in
            if dial > 0 && newDial == 0 && not clockwise then
                abs crossings + 1

            else if dial == 0 && not clockwise then
                abs crossings - 1

            else
                abs crossings
    in
    ( count + turns
    , newDial
    )


solvePart2 : String -> Int
solvePart2 input =
    input
        |> String.split "\n"
        |> List.map parseLine
        |> List.filterMap identity
        |> List.foldl step2 ( 0, 50 )
        |> Tuple.first


{-| ./run.mjs ./src/Day01.elm ../inputs/Day01.example
-}
main : Program String () ()
main =
    Platform.worker
        { init =
            \input ->
                let
                    _ =
                        Debug.log "Part 1" (solvePart1 input)

                    _ =
                        Debug.log "Part 2" (solvePart2 input)
                in
                ( ()
                , Cmd.none
                )
        , update =
            \() () ->
                ( ()
                , Cmd.none
                )
        , subscriptions =
            \() ->
                Sub.none
        }
