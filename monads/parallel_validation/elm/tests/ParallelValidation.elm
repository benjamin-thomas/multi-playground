module ParallelValidation exposing (testParallelValidation)

import Check
import Expect
import Test exposing (Test, test)


map2 : (a -> b -> c) -> Result appendable a -> Result appendable b -> Result appendable c
map2 f ra rb =
    case ( ra, rb ) of
        ( Err xa, Err xb ) ->
            Err (xa ++ xb)

        ( Err x, Ok _ ) ->
            Err x

        ( Ok _, Err x ) ->
            Err x

        ( Ok a, Ok b ) ->
            Ok <| f a b


andMap : Result x a -> Result (List x) (a -> c) -> Result (List x) c
andMap =
    map2 (\x f -> f x) << Result.mapError List.singleton


add : Int -> Int -> Result (List String) Int
add a b =
    Ok (\x y -> x + y)
        |> andMap (Check.positiveNumber a)
        |> andMap (Check.positiveNumber b)


testParallelValidation : Test
testParallelValidation =
    -- Computation will collect all errors before stopping or continuing
    let
        tests =
            [ \() -> 100 + 20 |> Expect.equal 120
            , \() -> add 10 1 |> Expect.equal (Ok 11)
            , \() -> add 0 10 |> Expect.equal (Err [ "Not positive: 0" ])
            , \() -> add 10 0 |> Expect.equal (Err [ "Not positive: 0" ])
            , \() -> add 0 -1 |> Expect.equal (Err [ "Not positive: -1", "Not positive: 0" ])
            ]
    in
    Test.concat <|
        List.indexedMap
            (\i f -> test ("test#" ++ String.fromInt i) f)
            tests
