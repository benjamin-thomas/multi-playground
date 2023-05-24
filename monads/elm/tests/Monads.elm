module Monads exposing (suite)

import Expect
import Test exposing (Test, describe, test)



{-

   elm-test --watch

-}


add : Maybe number -> Maybe number -> Maybe number -> Maybe number
add a b c =
    a
        |> Maybe.andThen
            (\x ->
                b
                    |> Maybe.andThen
                        (\y ->
                            c |> Maybe.andThen (\z -> Just (x + y + z))
                        )
            )


mul : Maybe number -> Maybe number -> Maybe number -> Maybe number
mul a b c =
    Maybe.map3 (\x y z -> x * y * z) a b c


suite : Test
suite =
    describe "main"
        [ test "Elm's formatter will heavily nest multiple andThen calls" <|
            \_ -> Expect.equal (Just 6) <| add (Just 1) (Just 2) (Just 3)
        , test "Solution: use map2, map3, etc." <|
            \_ -> Expect.equal (Just 24) <| mul (Just 4) (Just 3) (Just 2)
        , test "Just to reiterate, here's how to use map2" <|
            \_ -> Expect.equal (Just 3) <| Maybe.map2 (+) (Just 1) (Just 2)
        , test "Use the \"andMap\" pattern to handle arbritary args size" <|
            \_ ->
                -- https://thoughtbot.com/blog/running-out-of-maps
                let
                    andMap : Maybe a -> Maybe (a -> b) -> Maybe b
                    andMap =
                        Maybe.map2 (|>)
                in
                Expect.equal (Just 10)
                    (Just (\a b c d -> a + b + c + d)
                        |> andMap (Just 1)
                        |> andMap (Just 2)
                        |> andMap (Just 3)
                        |> andMap (Just 4)
                    )
        , test "this pattern should work for Result then" <|
            \_ ->
                let
                    andMap : Result x a -> Result x (a -> b) -> Result x b
                    andMap =
                        Result.map2 (|>)
                in
                Expect.equal (Ok 10)
                    (Ok (\a b c d -> a + b + c + d)
                        |> andMap (Ok 1)
                        |> andMap (Ok 2)
                        |> andMap (Ok 3)
                        |> andMap (Ok 4)
                    )
        , test "Error is returned if the computation pipeline fails at any point" <|
            \_ ->
                let
                    andMap : Result x a -> Result x (a -> b) -> Result x b
                    andMap =
                        Result.map2 (|>)
                in
                Expect.equal (Err "Oops @3")
                    -- NOTE: the arguments are processed in reverse order
                    (Ok (\a b c d -> a + b + c + d)
                        |> andMap (Ok 1)
                        |> andMap (Err "Oops @2")
                        |> andMap (Err "Oops @3")
                        |> andMap (Ok 4)
                    )
        ]
