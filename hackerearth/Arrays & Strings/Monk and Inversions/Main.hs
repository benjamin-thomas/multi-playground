{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (foldM, guard, replicateM, replicateM_)

{-

No error handling, I'm doing things quick and dirty here...

\$ cat ./input.txt | runghc ./Main.hs
0
2
17

ghci> :cmd pure $ unlines [":!clear", ":reload", "solution example"]

>>> solution example
17
 -}

example :: [[Int]]
example =
    [ [4, 7, 9]
    , [8, 2, 0]
    , [9, 1, 4]
    ]

type Matrix = [[Int]]
type Coord = (Int, Int)

focus :: Matrix -> Coord -> [Int]
focus matrix (y', x') = do
    (y, r) <- zip [0 ..] matrix
    (x, v) <- zip [0 ..] r
    guard (x >= x' && y >= y')
    pure v

coords :: Matrix -> [Coord]
coords matrix = do
    (y, r) <- zip [0 ..] matrix
    (x, _) <- zip [0 ..] r
    pure (y, x)

filterInversions :: (Ord a) => [a] -> [a]
filterInversions = \case
    [] -> []
    (x : xs) ->
        filter (< x) xs

solution :: Matrix -> Int
solution matrix = sum $ length . filterInversions . focus matrix <$> coords matrix

main :: IO ()
main = do
    cases <- read <$> getLine
    replicateM_ cases $ do
        handleCase

readNums :: String -> [Int]
readNums = fmap read . words

handleCase :: IO ()
handleCase = do
    dim :: Int <- read <$> getLine
    matrix <- replicateM dim $ readNums <$> getLine
    print $ solution matrix
