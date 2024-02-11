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


type Notice
    = Notice String


type AlwaysRun
    = AlwaysRun Bool


type alias SpecialCode =
    { repeat : Repeat
    , notice : Notice
    , alwaysRun : AlwaysRun
    }


repeatParser : Parser Repeat
repeatParser =
    P.succeed Repeat
        |. P.symbol "repeat="
        |= P.int


noticeParser : Parser Notice
noticeParser =
    P.succeed Notice
        |. P.keyword "notice"
        |. P.spaces
        |. P.symbol "="
        |. P.spaces
        |= P.oneOf
            [ P.symbol "=" |> P.andThen (\() -> P.problem "double eq!")
            , P.chompUntilEndOr "\n" |> P.getChompedString
            ]


noticeParserTest : Test
noticeParserTest =
    let
        test_ n ( inp, exp ) =
            test ("parse notice #" ++ String.fromInt n) <|
                \() -> P.run noticeParser inp |> Expect.equal exp
    in
    describe "noticeParserTest" <|
        List.indexedMap test_ <|
            [ ( "notice=Hello World!"
              , Ok (Notice "Hello World!")
              )
            , ( ""
              , Err [ { col = 1, problem = P.ExpectingKeyword "notice", row = 1 } ]
              )
            , ( "notice==Hello World!"
              , Err
                    [ { col = 9
                      , problem = P.Problem "double eq!"
                      , row = 1
                      }
                    ]
              )
            , ( "notice = Hello World!"
              , Ok (Notice "Hello World!")
              )
            ]


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


alwaysRunParser : Parser AlwaysRun
alwaysRunParser =
    let
        bool =
            P.chompUntilEndOr "\n"
                |> P.getChompedString
                |> P.andThen
                    (\str ->
                        case str of
                            "YES" ->
                                P.succeed True

                            "NO" ->
                                P.succeed False

                            _ ->
                                P.problem "Not in: [YES, NO]"
                    )
    in
    P.succeed AlwaysRun
        |. P.keyword "always"
        |. P.spaces
        |. P.symbol "="
        |. P.spaces
        |= P.oneOf
            [ P.symbol "=" |> P.andThen (\() -> P.problem "double eq!")
            , bool
            ]


alwaysRunParserTest : Test
alwaysRunParserTest =
    let
        test_ n ( inp, exp ) =
            test ("parse alwaysRun #" ++ String.fromInt n) <|
                \() -> P.run alwaysRunParser inp |> Expect.equal exp
    in
    describe "alwaysRunParserTest" <|
        List.indexedMap test_ <|
            [ ( ""
              , Err [ { col = 1, problem = P.ExpectingKeyword "always", row = 1 } ]
              )
            , ( "always=YES"
              , Ok (AlwaysRun True)
              )
            , ( "always=NO"
              , Ok (AlwaysRun False)
              )
            , ( "always =  YES"
              , Ok (AlwaysRun True)
              )
            , ( "always=NO!"
              , Err [ { col = 11, problem = P.Problem "Not in: [YES, NO]", row = 1 } ]
              )
            , ( "always==NO"
              , Err [ { col = 9, problem = P.Problem "double eq!", row = 1 } ]
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
                        Maybe.map2 (\num bool -> P.succeed (SpecialCode (Repeat num) (Notice alertMsg) bool))
                            (String.toInt nStr)
                            (case yn of
                                "YES" ->
                                    Just (AlwaysRun True)

                                "NO" ->
                                    Just (AlwaysRun False)

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
                            , notice = Notice "Some limit has been reached"
                            , alwaysRun = AlwaysRun True
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
                            , notice = Notice "Some limit has been reached"
                            , alwaysRun = AlwaysRun True
                            }
                        )
        , test "out of order keys parses" <|
            \() ->
                P.run parser "specialCode[alert=Reached bottom,repeat=5,always=NO]"
                    |> Expect.equal
                        (Ok
                            { repeat = Repeat 5
                            , notice = Notice "Reached bottom"
                            , alwaysRun = AlwaysRun False
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
