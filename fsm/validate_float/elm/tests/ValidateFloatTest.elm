module ValidateFloatTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import ValidateFloat exposing (State(..), init, run)


suite : Test
suite =
    describe "Valid floats"
        [ test "empty fails" <|
            \_ ->
                init (String.toList "")
                    |> run
                    |> Expect.equal { input = [], state = Fail }
        , test "x fails" <|
            \_ ->
                init (String.toList "x")
                    |> run
                    |> Expect.equal { input = [ 'x' ], state = Fail }
        , test "1 succeeds" <|
            \_ ->
                init (String.toList "1")
                    |> run
                    |> Expect.equal { input = [], state = Success }
        , test "12 succeeds" <|
            \_ ->
                init (String.toList "12")
                    |> run
                    |> Expect.equal { input = [], state = Success }
        , test "1x3 fails" <|
            \_ ->
                init (String.toList "1x3")
                    |> run
                    |> Expect.equal { input = [ 'x', '3' ], state = Fail }
        , test "12.34 succeeds" <|
            \_ ->
                init (String.toList "12.34")
                    |> run
                    |> Expect.equal { input = [], state = Success }
        , test "12.34. fails" <|
            \_ ->
                init (String.toList "12.34.")
                    |> run
                    |> Expect.equal { input = [ '.' ], state = Fail }
        , test "1. fails" <|
            \_ ->
                init (String.toList "1.")
                    |> run
                    |> Expect.equal { input = [], state = Fail }
        ]
