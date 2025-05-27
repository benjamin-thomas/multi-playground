module Main (main) where

import Data.Char (digitToInt)

{-

runghc ./Main.hs < <(echo "A2B1A2B2A1A2A2A2")

 -}

example :: String
example = "A2B1A2B2A1A2A2A2"

group :: [Char] -> [(Char, Int)]
group str = case str of
    [] -> []
    [x] -> error "bad data 1"
    team : score : rest ->
        (team, digitToInt score) : group rest

compute :: [(Char, Int)] -> (Int, Int)
compute =
    foldl tally (0, 0)
  where
    tally (aScore, bScore) (team, score) =
        case team of
            'A' ->
                (aScore + 1, bScore)
            'B' ->
                (aScore, bScore + 1)
            _ -> error "bad data 2"

main :: IO ()
main = do
    line <- getLine
    let (aScore, bScore) = compute $ group line
    putStrLn $ if aScore > bScore then "A" else "B"