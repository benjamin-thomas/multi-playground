module Day00 exposing (main)

import Platform


solvePart1 : String -> Int
solvePart1 _ =
    0


solvePart2 : String -> Int
solvePart2 _ =
    0


{-| ./run.mjs ./src/Day01.elm ../inputs/Day01.example
-}
main : Program String () ()
main =
    Platform.worker
        { init =
            \input ->
                let
                    _ =
                        Debug.log "Part 1" (solvePart1 input)

                    _ =
                        Debug.log "Part 2" (solvePart2 input)
                in
                ( ()
                , Cmd.none
                )
        , update =
            \() () ->
                ( ()
                , Cmd.none
                )
        , subscriptions =
            \() ->
                Sub.none
        }
