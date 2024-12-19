{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

{- cabal:

build-depends: base
-}

module Day04 where

import Control.Arrow (first)
import Data.List (transpose)
import Data.Tuple (swap)

example :: [String]
example =
    [ ['M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M']
    , ['M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A']
    , ['A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M']
    , ['M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X']
    , ['X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M']
    , ['X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A']
    , ['S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S']
    , ['S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A']
    , ['M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M']
    , ['M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X']
    ]

countXmas :: String -> Int
countXmas = \case
    [] -> 0
    ('X' : 'M' : 'A' : 'S' : xs) -> 1 + countXmas xs
    _ : xs -> countXmas xs

rot45 :: [[b]] -> [[b]]
rot45 lst = fmap (\(y, x) -> lst !! y !! x) <$> rot45coords lst

{-

Finds the (x,y) coordinates of a "rotated" list, at a "45" degree angle.
In other words, the diagonal indices.

3 !! 0

2 !! 0
3 !! 1

1 !! 0
2 !! 1
3 !! 2

0 !! 0
1 !! 1
2 !! 2
3 !! 3

0 !! 1
1 !! 2
2 !! 3

0 !! 2
1 !! 3

0 !! 3

 -}
rot45coords :: [a] -> [[(Int, Int)]]
rot45coords [] = []
rot45coords lst =
    let diag = [(x, x) | x <- [0 .. length lst - 1]]
     in diag : aux diag
  where
    aux [_] = []
    aux xs =
        let shiftRotL = first pred <$> tail xs
         in shiftRotL : fmap swap shiftRotL : aux shiftRotL

example0 :: [String]
example0 =
    [ ['A', 'B', 'C', 'D']
    , ['E', 'F', 'G', 'H']
    , ['I', 'J', 'K', 'L']
    , ['M', 'N', 'O', 'P']
    ]

main :: IO ()
main = do
    input <- readFile "../_inputs/04.txt"
    putStrLn "Part 1:"
    print $ answer1' input -- 2344
    putStrLn "---"
    putStrLn "Part 2:"
    putStrLn "TODO"

{-

1 2 3
4 5 6
7 8 9

 -}

answer1' :: String -> Int
answer1' lst = sum $ concat $ answer1 (lines lst)

{-

Debug with:

mapM_ print $ answer1 example

 -}
answer1 :: [String] -> [[Int]]
answer1 lst =
    let lstRev = reverse lst
        itemsRev = map reverse lst
     in fmap
            (fmap countXmas)
            [ lst
            , fmap reverse lst
            , rot45 lst
            , rot45 lstRev
            , rot45 itemsRev
            , rot45 (reverse itemsRev)
            , transpose lst
            , transpose lstRev
            ]
