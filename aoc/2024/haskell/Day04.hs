{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

{- cabal:

build-depends: base
-}

module Day04 where

import Control.Arrow (first)
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map
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

answer1' :: String -> Int
answer1' lst = sum $ concat $ answer1 (lines lst)

countXmas :: String -> Int
countXmas = \case
    [] -> 0
    ('X' : 'M' : 'A' : 'S' : xs) -> 1 + countXmas xs
    _ : xs -> countXmas xs

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

--------------------------------------------------------------------------------

kv :: [String] -> [((Int, Int), Char)]
kv str =
    [ ((x, y), c)
    | (y, row :: String) <- zip [0 ..] str
    , (x, c :: Char) <- zip [0 ..] row
    ]

{-

Debug with:

Map.mapWithKey (\xy _ -> fromEnum $ check' example2 xy) example2

 -}
example2 :: Map (Int, Int) Char
example2 = Map.fromList (kv lst)
  where
    lst :: [String]
    lst =
        [ ".M.S......"
        , "..A..MSMS." -- mapM_ print $ check example2 (2,1)
        , ".M.S.MAA.." -- (\row -> mas == row || mas == reverse row) <$> check example2 (2,1)
        , "..A.ASMSM."
        , ".M.S.M...."
        , ".........."
        , "S.S.S.S.S."
        , ".A.A.A.A.." -- (1,7)
        , "M.M.M.M.M."
        , ".........."
        ]

check :: Map (Int, Int) Char -> (Int, Int) -> [[Maybe Char]]
check dict (x, y) =
    let
        at = flip Map.lookup dict

        diagL :: [Maybe Char]
        diagL =
            [ at (x - 1, y - 1)
            , at (x, y)
            , at (x + 1, y + 1)
            ]

        diagR :: [Maybe Char]
        diagR =
            [ at (x - 1, y + 1)
            , at (x, y)
            , at (x + 1, y - 1)
            ]
     in
        [ diagL
        , diagR
        ]

mas :: [Maybe Char]
mas = fmap Just ['M', 'A', 'S']

check' :: Map (Int, Int) Char -> (Int, Int) -> Bool
check' dict (x, y) =
    all
        (\row -> mas == row || mas == reverse row)
        (check dict (x, y))

{-

>>> answer2 example2
9

 -}
answer2 :: Map (Int, Int) Char -> Int
answer2 dict =
    Map.foldr (+) 0 $ Map.mapWithKey (\xy _ -> fromEnum $ check' dict xy) dict

--------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- readFile "../_inputs/04.txt"
    putStrLn "Part 1:"
    print $ answer1' input -- 2344
    putStrLn "---"
    putStrLn "Part 2:"
    print $ answer2 $ Map.fromList (kv $ lines input) -- 1815