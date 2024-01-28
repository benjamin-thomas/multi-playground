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


testListParser =
    let
        parseList =
            P.chompWhile (\c -> c /= ']')
                |> P.getChompedString

        parser : Parser Int
        parser =
            P.succeed identity
                |. P.symbol "["
                |= (parseList
                        |> P.map (\str -> String.split "," str |> List.length)
                   )
    in
    describe "list parser"
        [ test "a" <|
            \() ->
                P.run parser "[12,b,c]"
                    |> Expect.equal (Ok 3)
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
