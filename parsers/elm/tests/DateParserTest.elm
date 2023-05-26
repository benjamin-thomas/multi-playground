module DateParserTest exposing (..)

import Expect
import Parser exposing ((|.), (|=), Parser, Problem(..), andThen, chompIf, chompWhile, getChompedString, int, map, problem, run, succeed, symbol)
import String
import Test exposing (..)


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


parseDigits : Test
parseDigits =
    let
        suceedOnDigit : String -> Parser Int
        suceedOnDigit str =
            case String.toInt str of
                Just x ->
                    succeed x

                Nothing ->
                    problem <| str ++ " is not a digit!"

        digit : Parser Int
        digit =
            chompIf Char.isDigit
                |> getChompedString
                |> andThen suceedOnDigit

        digit2 : Parser Int
        digit2 =
            chompIf Char.isDigit
                |> getChompedString
                |> andThen
                    (\first ->
                        chompIf Char.isDigit
                            |> getChompedString
                            |> andThen
                                (\second ->
                                    suceedOnDigit (first ++ second)
                                )
                    )
    in
    describe "parseDigits"
        [ test "parse 1 digit" <|
            \() ->
                run digit "1x"
                    |> Expect.equal (Ok 1)
        , test "parse 2 digits" <|
            \() ->
                run digit2 "12x"
                    |> Expect.equal (Ok 12)
        ]


dateParser : Test
dateParser =
    let
        -- I cannot use the builtin `int` parser will fail on "1.23" (and my number separartor is "."!)
        digit : Parser Int
        digit =
            chompWhile Char.isDigit
                |> getChompedString
                |> andThen
                    (\str ->
                        case String.toInt str of
                            Just n ->
                                succeed n

                            Nothing ->
                                problem <| str ++ " is not a number!"
                    )

        parser : Parser Date
        parser =
            succeed Date
                |= digit
                |. symbol "."
                |= digit
                |. symbol "."
                |= digit
    in
    describe "dateParser"
        [ test "valid input" <|
            \() ->
                run parser "2023.05.26"
                    |> Expect.equal (Ok { year = 2023, month = 5, day = 26 })

        -- NOTE: I don't know yet how to produce better error messages!
        , test "invalid year" <|
            \() ->
                run parser "2023x.05.26"
                    |> Expect.equal (Err [ { col = 5, problem = ExpectingSymbol ".", row = 1 } ])
        , test "missing day" <|
            \() ->
                run parser "2023.05"
                    |> Expect.equal (Err [ { col = 8, problem = ExpectingSymbol ".", row = 1 } ])
        ]


exploreSuite : Test
exploreSuite =
    let
        parser : Parser Int
        parser =
            succeed identity
                |= int
    in
    describe "exploring"
        [ test "parse 1 digit" <|
            \() ->
                run parser "1"
                    |> Expect.equal (Ok 1)
        , test "parse 2 digits" <|
            \() ->
                run parser "12"
                    |> Expect.equal (Ok 12)
        , test "non-digit at start" <|
            \() ->
                run parser "x12"
                    |> Expect.equal
                        (Err
                            [ { col = 1
                              , problem = ExpectingInt
                              , row = 1
                              }
                            ]
                        )
        , test "non-digit in the middle" <|
            \() ->
                run parser "1x2"
                    |> Expect.equal (Ok 1)
        ]
