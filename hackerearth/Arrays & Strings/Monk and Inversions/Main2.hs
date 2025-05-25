{-# LANGUAGE LambdaCase #-}

module Main2 () where

import Control.Monad (foldM, guard, replicateM, replicateM_)
import Data.Array

{-

Same as Main.hs, but using Array

 -}

exampleList :: [[Int]]
exampleList =
    [ [4, 7, 9]
    , [8, 2, 0]
    , [9, 1, 4]
    ]

example :: Array (Int, Int) Int
example = fromList exampleList

fromList :: [[v]] -> Array (Int, Int) v
fromList lst =
    listArray
        ((0, 0), (rows - 1, cols - 1))
        (concat lst)
  where
    rows = length lst
    cols = length . head $ lst

type Matrix = Array (Int, Int) Int
type Coord = (Int, Int)

focus :: Matrix -> Coord -> [Int]
focus matrix (y', x') = do
    ((y, x), v) <- assocs matrix
    guard (x >= x' && y >= y')
    pure v

solution :: Matrix -> Int
solution matrix = sum $ length . filterInversions . focus matrix <$> indices matrix

filterInversions :: (Ord a) => [a] -> [a]
filterInversions = \case
    [] -> []
    (x : xs) -> filter (< x) xs

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
    print . solution $ fromList matrix
