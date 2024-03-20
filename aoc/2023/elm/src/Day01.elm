module Day01 exposing (filterMap, filterMapNumsPlus, filterNumbers, keepOuter, part1, part2, program)

import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc



{-
   cat ../_inputs/day01.txt | elm-cli run src/Day01.elm
-}


filterNumbers : List Char -> List Char
filterNumbers =
    List.filter Char.isDigit


filterMapNumsPlus : List Char -> List Int
filterMapNumsPlus lst =
    let
        aux acc lst_ =
            case lst_ of
                [] ->
                    List.reverse acc

                'o' :: 'n' :: 'e' :: xs ->
                    aux (1 :: acc) ('e' :: xs)

                't' :: 'w' :: 'o' :: xs ->
                    aux (2 :: acc) ('o' :: xs)

                't' :: 'h' :: 'r' :: 'e' :: 'e' :: xs ->
                    aux (3 :: acc) ('e' :: xs)

                'f' :: 'o' :: 'u' :: 'r' :: xs ->
                    aux (4 :: acc) ('r' :: xs)

                'f' :: 'i' :: 'v' :: 'e' :: xs ->
                    aux (5 :: acc) ('e' :: xs)

                's' :: 'i' :: 'x' :: xs ->
                    aux (6 :: acc) ('x' :: xs)

                's' :: 'e' :: 'v' :: 'e' :: 'n' :: xs ->
                    aux (7 :: acc) ('n' :: xs)

                'e' :: 'i' :: 'g' :: 'h' :: 't' :: xs ->
                    aux (8 :: acc) ('t' :: xs)

                'n' :: 'i' :: 'n' :: 'e' :: xs ->
                    aux (9 :: acc) ('e' :: xs)

                x :: xs ->
                    if Char.isDigit x then
                        aux (toInt x :: acc) xs

                    else
                        aux acc xs
    in
    aux [] lst


toInt : Char -> Int
toInt c =
    Char.toCode c - Char.toCode '0'


keepOuter : List a -> Maybe ( a, a )
keepOuter lst =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            List.foldl (\y _ -> Just ( x, y ))
                (Just ( x, x ))
                xs


filterMap : List String -> List ( Int, Int )
filterMap lines =
    List.map (Tuple.mapBoth toInt toInt) <|
        List.filterMap (keepOuter << filterNumbers << String.toList) lines


sumLines : List ( number, number ) -> number
sumLines =
    List.foldl (\( tens, units ) acc -> acc + tens * 10 + units) 0


part1 : List String -> Int
part1 lines =
    filterMap lines
        |> sumLines


part2 : List String -> Int
part2 lines =
    lines
        |> List.filterMap (String.toList >> filterMapNumsPlus >> keepOuter)
        |> sumLines


mkPrint : String -> (List String -> Int) -> String -> () -> IO ()
mkPrint header f content () =
    Proc.print (header ++ ": " ++ (String.fromInt <| f <| String.lines content))


program : Process -> IO ()
program _ =
    File.read File.stdIn
        |> IO.andThen
            (\content ->
                IO.return ()
                    |> IO.andThen (mkPrint "Part 1" part1 content)
                    |> IO.andThen (mkPrint "Part 2" part2 content)
            )
