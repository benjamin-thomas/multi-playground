module Day03Test exposing (..)

import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)


example : String
example =
    """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
    """


genRow : ( a, String ) -> List ( ( Int, a ), Char )
genRow ( col, str ) =
    str
        |> String.toList
        |> List.indexedMap
            (\row c ->
                ( ( row, col ), c )
            )


wip str =
    str
        |> String.trim
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.concatMap genRow
        |> List.filter (\( _, c ) -> c /= '.')
        |> Dict.fromList
        |> Debug.log "WIP"


wipTest =
    test "WIP" <|
        \_ ->
            let
                _ =
                    wip example
            in
            Expect.pass


table : String -> List (List Char)
table str =
    str
        |> String.trim
        |> String.lines
        |> List.map String.toList



{-

   In this schematic, two numbers are not part numbers because they are not adjacent to a symbol:

   114 (top right) and 58 (middle right).

   Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

-}


tableTest : Test
tableTest =
    describe "Table"
        [ test "example" <|
            \_ ->
                table example
                    |> Expect.equal
                        [ [ '4', '6', '7', '.', '.', '1', '1', '4', '.', '.' ]
                        , [ '.', '.', '.', '*', '.', '.', '.', '.', '.', '.' ]
                        , [ '.', '.', '3', '5', '.', '.', '6', '3', '3', '.' ]
                        , [ '.', '.', '.', '.', '.', '.', '#', '.', '.', '.' ]
                        , [ '6', '1', '7', '*', '.', '.', '.', '.', '.', '.' ]
                        , [ '.', '.', '.', '.', '.', '+', '.', '5', '8', '.' ]
                        , [ '.', '.', '5', '9', '2', '.', '.', '.', '.', '.' ]
                        , [ '.', '.', '.', '.', '.', '.', '7', '5', '5', '.' ]
                        , [ '.', '.', '.', '$', '.', '*', '.', '.', '.', '.' ]
                        , [ '.', '6', '6', '4', '.', '5', '9', '8', '.', '.' ]
                        ]
        ]


neighbours : { x : Int, y : Int, width : Int, height : Int } -> List ( Int, Int )
neighbours { x, y, width, height } =
    [ -- top
      ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )

    -- middle
    , ( x - 1, y )
    , ( x + 1, y )

    -- bottom
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.filter (\( x_, y_ ) -> x_ >= 0 && x_ < width && y_ >= 0 && y_ < height)
