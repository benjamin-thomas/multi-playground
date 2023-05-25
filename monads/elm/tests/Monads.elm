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

                    -- https://nicolas.perriault.net/code/2021/elm-result-pipeline/
                    apply : Result x a -> Result x (a -> b) -> Result x b
                    apply result =
                        Result.andThen (\partial -> Result.map partial result)

                    apply2 x fn =
                        Result.map fn x
                in
                Expect.equal (Ok 28)
                    (Ok (\a b c d e f g -> a + b + c + d + e + f + g)
                        |> andMap (Ok 1)
                        |> andMap (Ok 2)
                        |> apply (Ok 3)
                        |> Result.andThen (\fn -> Result.map fn (Ok 4))
                        |> Result.andThen (\fn -> Ok 5 |> Result.map fn)
                        |> Result.andThen (apply2 (Ok 6))
                        |> Result.map2 (\a b -> b a) (Ok 7)
                    )
        , test "Error is returned if the computation pipeline fails at any point" <|
            \() ->
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
        , test "let's apply the elm-result-pipeline style to Maybe" <|
            \() ->
                let
                    apply : Maybe a -> Maybe (a -> b) -> Maybe b
                    apply m =
                        Maybe.andThen (\partial -> Maybe.map partial m)
                in
                Expect.equal (Just 13)
                    (Just (\a b c -> a * b + c)
                        |> apply (Just 3)
                        |> apply (Just 4)
                        |> apply (Just 1)
                    )
        , test "this method processes the args in a more logical order (see `andMap` above)" <|
            \_ ->
                let
                    -- https://nicolas.perriault.net/code/2021/elm-result-pipeline/
                    apply : Result x a -> Result x (a -> b) -> Result x b
                    apply result =
                        Result.andThen (\partial -> Result.map partial result)
                in
                Expect.equal (Err "2")
                    (Ok (\a b c d -> a + b + c + d)
                        |> apply (Ok 1)
                        |> apply (Err "2")
                        |> apply (Err "3")
                        |> apply (Ok 4)
                    )
        , test "verbose style" <|
            \() ->
                let
                    myComputation =
                        Ok (\a b c -> a + b + c)
                            |> Result.andThen (\fn -> Result.map fn (Ok 1))
                            |> Result.andThen (\fn -> Result.map fn (Ok 2))
                            |> Result.andThen (\fn -> Result.map fn (Ok 3))
                in
                Expect.equal (Ok 6) myComputation
        , test "in verbose style, the computation does stop on the first error" <|
            \() ->
                Expect.equal (Err "2")
                    (Ok (\a b c -> a + b + c)
                        |> Result.andThen (\fn -> Result.map fn (Ok 1))
                        |> Result.andThen (\fn -> Result.map fn (Err "2"))
                        |> Result.andThen (\fn -> Result.map fn (Err "3"))
                    )
        , test "simplify" <|
            \() ->
                let
                    myComputation =
                        Ok identity |> Result.andThen (\fn -> Result.map fn (Ok 1))
                in
                Expect.equal (Ok 1) myComputation
        , test "reversing the flow re-introduces indenting by the formatter" <|
            \() ->
                let
                    apply x =
                        Result.andThen (\partial -> Result.map partial x)
                in
                Expect.equal (Ok 13)
                    (apply (Ok 1) <|
                        apply (Ok 4) <|
                            apply (Ok 3) <|
                                Ok (\a b c -> a * b + c)
                    )
        ]
