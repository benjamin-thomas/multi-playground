module SequentialValidation exposing (testSequentialValidation)

import Check
import Expect
import Test exposing (Test, test)


andMap : Result x a -> Result x (a -> b) -> Result x b
andMap =
    Result.map2 (\x f -> f x)


add : Int -> Int -> Result String Int
add a b =
    Ok (\x y -> x + y)
        |> andMap (Check.positiveNumber a)
        |> andMap (Check.positiveNumber b)


testSequentialValidation : Test
testSequentialValidation =
    -- Computation will stop on the first error
    let
        tests =
            [ \() -> 100 + 20 |> Expect.equal 120
            , \() -> add 1 10 |> Expect.equal (Ok 11)
            , \() -> add 0 33 |> Expect.equal (Err "Not positive: 0")
            , \() -> add 1 -3 |> Expect.equal (Err "Not positive: -3")
            , \() -> add 0 -3 |> Expect.equal (Err "Not positive: -3")
            ]
    in
    Test.concat <|
        List.indexedMap
            (\i f -> test ("test#" ++ String.fromInt i) f)
            tests
