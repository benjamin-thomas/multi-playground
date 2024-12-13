{-# OPTIONS_GHC -Wall -Wextra #-}

module Main (main, example) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (delete)
import Text.Printf (printf)

{-

:cmd return $ unlines [":!clear",":reload", "toTup2 example"]

 -}
example :: String
example =
    unlines
        [ "3   4"
        , "4   3"
        , "2   5"
        , "1   3"
        , "3   9"
        , "3   3"
        ]

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

{-

>>> toTup2 example
[(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]

>>> sorted $ toTup2 example
[(3,1),(3,2),(3,3),(4,3),(5,3),(9,4)]

 -}
toTup2 :: String -> [(Int, Int)]
toTup2 str =
    makePair . fmap read . words <$> lines str
  where
    makePair [a, b] = (a, b)
    makePair _ = error "bad data"

sorted :: [(Int, Int)] -> [(Int, Int)]
sorted [] = []
sorted tups =
    let
        as = fst <$> tups
        bs = snd <$> tups

        minA = minimum as
        minB = minimum bs

        newAs = delete minA as
        newBs = delete minB bs

        header = if minA < minB then (minB, minA) else (minA, minB)
     in
        header : sorted (zip newAs newBs)

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

{-

:cmd return $ unlines [":!clear",":reload", "similarity $ toTup2 example"]
>>> toTup2 example
[(3,4),(4,3),(2,5),(1,3),(3,9),(3,3)]

So, for these example lists, the similarity score at the end of this process is 31 (9 + 4 + 0 + 0 + 9 + 9).

>>> similarity $ toTup2 example
[(3,9),(4,0),(2,0),(1,0),(3,3),(3,0)]

 -}
similarity :: [(Int, Int)] -> [Int]
similarity [] = []
similarity tups =
    let
        as = fst <$> tups
        bs = snd <$> tups

        occurrences = count bs <$> as
     in
        occurrences
  where
    count bs a = sum $ filter (== a) bs

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

{-
>>> answer1 example
[2,1,0,1,2,5]
>>> answer1 example & sum
11
-}
answer1 :: String -> [Int]
answer1 inp = toTup2 inp & sorted <&> uncurry (-)

{-

>>> answer2 example
[9,4,0,0,9,9]

 -}
answer2 :: String -> [Int]
answer2 inp = toTup2 inp & similarity

main :: IO ()
main = do
    inp <- readFile "input1.txt"
    let ans1 = answer1 inp & sum
    printf "Answer 1: %d\n" ans1 -- 1189304
    let ans2 = answer2 inp & sum
    printf "Answer 2: %d\n" ans2
