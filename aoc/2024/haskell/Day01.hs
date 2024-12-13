{-# OPTIONS_GHC -Wall -Wextra #-}

module Day01 (main, example) where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (delete, sort)
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

 -}
toTup2 :: String -> [(Int, Int)]
toTup2 str =
    makePair . fmap read . words <$> lines str
  where
    makePair [a, b] = (a, b)
    makePair _ = error "bad data"

{-

>>> inTwo example
([3,4,2,1,3,3],[4,3,5,3,9,3])

 -}
inTwo :: String -> ([Int], [Int])
inTwo = unzip . toTup2

{-
>>> sorted $ toTup2 example
[(3,1),(3,2),(3,3),(4,3),(5,3),(9,4)]

 -}
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

{-

>>> sorted' $ inTwo example
([1,2,3,3,3,4],[3,3,3,4,5,9])

 -}
sorted' :: ([Int], [Int]) -> ([Int], [Int])
sorted' = bimap sort sort

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

{-

>>> inTwo example
([3,4,2,1,3,3],[4,3,5,3,9,3])

>>> similarity' $ inTwo example
[9,4,0,0,9,9]

 -}
similarity' :: ([Int], [Int]) -> [Int]
similarity' ([], _) = []
similarity' (_, []) = []
similarity' (as, bs) =
    count <$> as
  where
    count a = sum $ filter (== a) bs

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

>>> answer1' example
[2,1,0,1,2,5]
>>> answer1' example & sum
11
 -}
answer1' :: String -> [Int]
answer1' inp =
    let xs = sorted' $ inTwo inp
     in (\(a, b) -> if a > b then a - b else b - a) <$> uncurry zip xs

{-

>>> answer2 example
[9,4,0,0,9,9]

 -}
answer2 :: String -> [Int]
answer2 inp = toTup2 inp & similarity

{-

>>> answer2' example
[9,4,0,0,9,9]

 -}
answer2' :: String -> [Int]
answer2' inp = similarity' $ inTwo inp

main :: IO ()
main = do
    inp <- readFile "../_inputs/01.txt"
    let ex1 = answer1 example & sum
    let ex1' = answer1' example & sum
    printf "Example 1: (%d,%d)\n" ex1 ex1' -- 11
    let ans1 = answer1 inp & sum
    let ans1' = answer1' inp & sum
    printf "Answer 1: (%d,%d)\n" ans1 ans1' -- 1189304
    let ans2 = answer2 inp & sum
    let ans2' = answer2' inp & sum
    printf "Answer 2: (%d,%d)\n" ans2 ans2' -- 24349736
    -- putStrLn "DEBUG"
    -- print $ take 10 (answer1 inp)
    -- print $ take 10 (answer1' inp)
