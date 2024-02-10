module DateParserTest exposing (..)

import Expect
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Problem(..)
        , andThen
        , chompIf
        , chompWhile
        , getChompedString
        , int
        , problem
        , run
        , succeed
        , symbol
        )
import String
import Test exposing (..)


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


parseDigits2 : Test
parseDigits2 =
    let
        validateDigit : Int -> String -> Parser Int
        validateDigit cnt str =
            case String.toInt str of
                Nothing ->
                    problem <| str ++ " is not a digit!"

                Just x ->
                    let
                        len =
                            String.length str
                    in
                    if len /= cnt then
                        problem <|
                            "Expected digit of length "
                                ++ String.fromInt cnt
                                ++ ", not "
                                ++ String.fromInt len

                    else
                        succeed x

        digits : Int -> Parser Int
        digits cnt =
            chompWhile Char.isDigit
                |> getChompedString
                |> andThen (validateDigit cnt)
    in
    describe "parseDigits2"
        [ test "parse 1 digit" <|
            \() ->
                run (digits 1) "1x"
                    |> Expect.equal (Ok 1)
        , test "parse 2 digits" <|
            \() ->
                run (digits 2) "12x"
                    |> Expect.equal (Ok 12)
        , test "parse 4 digits" <|
            \() ->
                run (digits 4) "1234."
                    |> Expect.equal (Ok 1234)
        , test "fails if input has more" <|
            \() ->
                run (digits 4) "12345x"
                    |> Expect.equal
                        (Err
                            [ { col = 6
                              , problem = Problem "Expected digit of length 4, not 5"
                              , row = 1
                              }
                            ]
                        )
        , test "fails if input has less" <|
            \() ->
                run (digits 4) "123x4"
                    |> Expect.equal
                        (Err
                            [ { col = 4
                              , problem = Problem "Expected digit of length 4, not 3"
                              , row = 1
                              }
                            ]
                        )
        , test "I should now be able to parse a date" <|
            let
                dateParser2 sep =
                    succeed Date
                        |= digits 4
                        |. symbol sep
                        |= digits 2
                        |. symbol sep
                        |= digits 2
            in
            \() ->
                run (dateParser2 ".") "2023.05.26"
                    |> Expect.equal
                        (Ok { year = 2023, month = 5, day = 26 })
        , test "hyphen sep, I'll clean up thet tests later..." <|
            let
                dateParser2 sep =
                    succeed Date
                        |= digits 4
                        |. symbol sep
                        |= digits 2
                        |. symbol sep
                        |= digits 2
            in
            \() ->
                run (dateParser2 "-") "2023-05-26"
                    |> Expect.equal
                        (Ok { year = 2023, month = 5, day = 26 })
        ]


parseDigits : Test
parseDigits =
    {- This whole approach is overcomplicated. Plus the falsey branch will never trigger... -}
    let
        succeedOnDigit : String -> Parser Int
        succeedOnDigit str =
            case String.toInt str of
                Just x ->
                    succeed x

                Nothing ->
                    problem <| str ++ " is not a digit!"

        digit : Parser Int
        digit =
            chompIf Char.isDigit
                |> getChompedString
                |> andThen succeedOnDigit

        digit2Verbose : Parser Int
        digit2Verbose =
            chompIf Char.isDigit
                |> getChompedString
                |> andThen
                    (\first ->
                        chompIf Char.isDigit
                            |> getChompedString
                            |> andThen
                                (\second ->
                                    succeedOnDigit (first ++ second)
                                )
                    )

        digitStr : Parser String
        digitStr =
            chompIf Char.isDigit
                |> getChompedString

        digitPair : Parser Int
        digitPair =
            (succeed Tuple.pair
                |= digitStr
                |= digitStr
            )
                |> andThen (\( a, b ) -> succeedOnDigit (a ++ b))
    in
    describe "parseDigits"
        [ test "parse 1 digit" <|
            \() ->
                run digit "1x"
                    |> Expect.equal (Ok 1)
        , test "parse 2 digits" <|
            \() ->
                run digit2Verbose "12x"
                    |> Expect.equal (Ok 12)
        , test "parse 2 digits, again" <|
            \() ->
                run digitPair "12x"
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
