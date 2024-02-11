module SpecialCodeParser exposing (..)

import Expect
import Parser as P
    exposing
        ( (|.)
        , (|=)
        , Parser
        )
import Test exposing (Test, describe, test)


testListParser : Test
testListParser =
    let
        innerListParser : Parser (List String)
        innerListParser =
            P.chompWhile (\c -> c /= ']')
                |> P.getChompedString
                |> P.map (\str -> str |> String.split "," |> List.map String.trim)

        stringListParser : Parser (List String)
        stringListParser =
            P.succeed identity
                |. P.symbol "["
                |= innerListParser

        intListParser : Parser (List Int)
        intListParser =
            P.succeed identity
                |. P.symbol "["
                |= (innerListParser |> P.map (List.map (String.toInt >> Maybe.withDefault 0)))
    in
    describe "list parsers"
        [ test "strings" <|
            \() ->
                P.run stringListParser "[ 1, 2 , 3 ]"
                    |> Expect.equal (Ok [ "1", "2", "3" ])
        , test "ints" <|
            \() ->
                P.run intListParser "[ 1, 2 , 3 ]"
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        ]


type Repeat
    = Repeat Int


type alias SpecialCode =
    { repeat : Repeat
    , notice : String
    , alwaysRun : Bool
    }


repeatParser : Parser Repeat
repeatParser =
    P.succeed Repeat
        |. P.symbol "repeat="
        |= P.int


repeatParserTest : Test
repeatParserTest =
    let
        test_ n ( inp, exp ) =
            test ("parse repeat #" ++ String.fromInt n) <|
                \() -> P.run repeatParser inp |> Expect.equal exp
    in
    describe "repeatParserTest" <|
        List.indexedMap test_ <|
            [ ( ""
              , Err [ { col = 1, problem = P.ExpectingSymbol "repeat=", row = 1 } ]
              )
            , ( "repeat=123"
              , Ok (Repeat 123)
              )
            , ( "repeaT=123"
              , Err [ { col = 1, problem = P.ExpectingSymbol "repeat=", row = 1 } ]
              )
            , ( "wat=123"
              , Err [ { col = 1, problem = P.ExpectingSymbol "repeat=", row = 1 } ]
              )
            , ( "repeat==123"
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            , ( "repeat=a123"
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            , ( "repeat=\"123\""
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            , ( "repeat='123'"
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            , ( "repeat=4"
              , Ok (Repeat 4)
              )
            , ( "repeat=4."
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            , ( "repeat=4.5"
              , Err [ { col = 8, problem = P.ExpectingInt, row = 1 } ]
              )
            ]


parser : Parser SpecialCode
parser =
    let
        checkElements : String -> Parser SpecialCode
        checkElements str =
            let
                -- TODO: use P.oneOf [codeA, codeB, etc.]
                keysAndValues : List (List String)
                keysAndValues =
                    String.split "," str
                        |> List.map String.trim
                        |> List.sort
                        |> List.map (String.split "=" >> List.map String.trim)
            in
            case keysAndValues of
                [ [ "alert", alertMsg ], [ "always", yn ], [ "repeat", nStr ] ] ->
                    Maybe.withDefault (P.problem "One or more invalid values (or keys)") <|
                        Maybe.map2 (\num bool -> P.succeed (SpecialCode (Repeat num) alertMsg bool))
                            (String.toInt nStr)
                            (case yn of
                                "YES" ->
                                    Just True

                                "NO" ->
                                    Just False

                                _ ->
                                    Nothing
                            )

                _ ->
                    P.problem "One or more invalid keys (not in [alert,always,repeat])"

        parseList : Parser SpecialCode
        parseList =
            P.getChompedString
                (P.chompWhile (\c -> c /= ']'))
                |> P.andThen checkElements

        parser_ : Parser SpecialCode
        parser_ =
            P.succeed identity
                |. P.symbol "specialCode"
                |. P.symbol "["
                |= parseList
    in
    parser_


specialCodeParser : Test
specialCodeParser =
    describe "specialCodeParser"
        [ test "valid input" <|
            \() ->
                P.run parser "specialCode[repeat=3,alert=Some limit has been reached,always=YES]"
                    |> Expect.equal
                        (Ok
                            { repeat = Repeat 3
                            , notice = "Some limit has been reached"
                            , alwaysRun = True
                            }
                        )
        , test "invalidInput" <|
            \() ->
                P.run parser "wat"
                    |> Expect.equal
                        (Err
                            [ { col = 1
                              , problem = P.ExpectingSymbol "specialCode"
                              , row = 1
                              }
                            ]
                        )
        , test "unclean input parses correctly" <|
            \() ->
                P.run parser "specialCode[ repeat =4, alert = Some limit has been reached,  always= YES  ]"
                    |> Expect.equal
                        (Ok
                            { repeat = Repeat 4
                            , notice = "Some limit has been reached"
                            , alwaysRun = True
                            }
                        )
        , test "out of order keys parses" <|
            \() ->
                P.run parser "specialCode[alert=Reached bottom,repeat=5,always=NO]"
                    |> Expect.equal
                        (Ok
                            { repeat = Repeat 5
                            , notice = "Reached bottom"
                            , alwaysRun = False
                            }
                        )
        , test "rejects bad values" <|
            \() ->
                P.run parser "specialCode[alert=wat,repeat=5,always=NOO!]"
                    |> Expect.equal
                        (Err
                            [ { col = 43
                              , problem = P.Problem "One or more invalid values (or keys)"
                              , row = 1
                              }
                            ]
                        )
        , test "rejects bad keys" <|
            \() ->
                P.run parser "specialCode[hey=wat,repeat=5,always=YES]"
                    |> Expect.equal
                        (Err
                            [ { col = 40
                              , problem = P.Problem "One or more invalid keys (not in [alert,always,repeat])"
                              , row = 1
                              }
                            ]
                        )
        ]
