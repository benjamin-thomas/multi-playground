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
        ]
