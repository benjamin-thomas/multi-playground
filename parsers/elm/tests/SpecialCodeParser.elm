module SpecialCodeParser exposing (..)

{-

   WIP: I'm kinda clueless here!
-}

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


type alias SpecialCode =
    { repeat : Int
    , notice : String
    , alwaysRun : Bool
    }


specialCodeParser : Test
specialCodeParser =
    let
        -- parseItem _ =
        --     succeed 3
        --         |. symbol "répéter="
        --         |= int
        checkElements : String -> Parser Int
        checkElements str =
            case String.split "," str of
                [ a, b, c ] ->
                    P.succeed 0

                _ ->
                    P.problem "Invalid number of elements"

        parseList : Parser Int
        parseList =
            P.getChompedString (P.chompWhile (\c -> c /= ']'))
                |> P.andThen checkElements

        parser : Parser SpecialCode
        parser =
            P.succeed (\x -> SpecialCode x "wat" True)
                |. P.symbol "myCode"
                |. P.symbol "["
                |= parseList
    in
    describe "specialCodeParser"
        [ test "valid input" <|
            \() ->
                P.run parser "myCode[répéter=3,alerte=Le seuil a été dépassé,tjs=OUI]"
                    |> Expect.equal (Ok { repeat = 0, notice = "wat", alwaysRun = True })

        -- , test "invalidInput" <|
        --     \() ->
        --         run parser "wat"
        --             |> Expect.equal (Err [ { col = 1, problem = ExpectingSymbol "myCode", row = 1 } ])
        ]
