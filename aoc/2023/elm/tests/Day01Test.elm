module Day01Test exposing (part1Suite, part2Suite)

import Day01
import Expect
import Test exposing (Test, describe, test)


part1Suite : Test
part1Suite =
    describe "Part 1"
        [ test "filterNumbers" <|
            \_ ->
                Day01.filterNumbers (String.toList "1abc2pqr3stu8vwx")
                    |> Expect.equal [ '1', '2', '3', '8' ]
        , describe "keepOuter" <|
            [ test "empty" <|
                \_ ->
                    Day01.keepOuter [] |> Expect.equal Nothing
            , test "one item" <|
                \_ ->
                    Day01.keepOuter [ 1 ]
                        |> Expect.equal (Just ( 1, 1 ))
            , test "several items" <|
                \_ ->
                    Day01.keepOuter [ 1, 2, 3 ]
                        |> Expect.equal (Just ( 1, 3 ))
            ]
        , test "filterMap" <|
            \_ ->
                Day01.filterMap [ "1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet" ]
                    |> Expect.equal [ ( 1, 2 ), ( 3, 8 ), ( 1, 5 ), ( 7, 7 ) ]
        , test "part1" <|
            \_ ->
                Day01.part1 [ "1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet" ]
                    |> Expect.equal 142
        ]


part2Suite : Test
part2Suite =
    describe "Part 2"
        [ test "filterMoreNumbers" <|
            \_ ->
                Day01.filterMapNumsPlus (String.toList "abcone2threeightxyz")
                    |> Expect.equal [ 1, 2, 3, 8 ]
        ]
