{- cabal:

build-depends: base

-}
{-# OPTIONS_GHC -Wall -Wextra #-}

{-

NOTE: the pretty printer requires a minimal cabal script.
      If you do require dependencies, just create a normal cabal project, as the
      vscode plugin is not aware of cabal scripts dependencies.

Terminal 1 (without or with pretty printer)
\$ ghci
\$ cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple ./Day02.hs
> :cmd return $ unlines [":!clear",":reload", "answer1 example"]

Terminal 2
find *.hs | entr tmux send-keys -t aoc:0 Up Enter
 -}

module Day02 (main, example) where

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

answer1 :: String -> Int
answer1 = length . filter isSafe . toInts

answer2 :: String -> Int
answer2 str =
    length $ filter (any isSafe) $ expand <$> toInts str

{- FOURMOLU_DISABLE -}
{-
[18] pry(main)> lst = [2,4,6,8]
=> [2, 4, 6, 8]
[19] pry(main)> lst.filter.with_index { |_, i2| i2 != 0 }
=> [4, 6, 8]
[20] pry(main)> lst.filter.with_index { |_, i2| i2 != 2 }
=> [2, 4, 8]

*Day02> lst = [2,4,6,8]
*Day02> map snd $ filter (\(i, _) -> i /= 0) $ zip [0..] lst
[4,6,8]
*Day02> map snd $ filter (\(i, _) -> i /= 2) $ zip [0..] lst
[2,4,8]

 -}
{- FOURMOLU_ENABLE -}

removeAt :: (Eq idx, Num idx, Enum idx) => [a] -> idx -> [a]
removeAt lst idx =
    map snd $ filter (\(i, _) -> i /= idx) $ zip [0 ..] lst

-- expand lst = map (\idx -> removeAt idx lst) $ map fst $ zip [0 ..] lst
expand :: [a] -> [[a]]
expand lst = removeAt lst <$> indices
  where
    indices :: [Int]
    indices = fst <$> zip [0 ..] lst

main :: IO ()
main = do
    input <- readFile "../_inputs/02.txt"
    print $ answer1 input -- 421
    print $ answer2 input -- 476

toInts :: String -> [[Int]]
toInts str = mapMaybe toInt . words <$> lines str
  where
    toInt :: String -> Maybe Int
    toInt = readMaybe

example :: String
example =
    unlines
        [ "7 6 4 2 1" -- dec OK
        , "1 2 7 8 9" -- too big, 2 to 7 = 5
        , "9 7 6 2 1" -- too big, 6 to 2 = 4
        , "1 3 2 4 5" -- inc/dec, bad
        , "8 6 4 4 1" -- stationary 4 to 4 = 0
        , "1 3 6 7 9" -- inc OK
        ]

delta :: [Int] -> [Int]
delta = zipWith (flip (-)) <*> tail

allAsc :: [Int] -> Bool
allAsc = all (> 0) . delta

allDesc :: [Int] -> Bool
allDesc = all (< 0) . delta

oneWay :: [Int] -> Bool
oneWay = liftA2 (||) allAsc allDesc

inRange :: [Int] -> Bool
inRange lst = all ((\n -> n > 0 && n < 4) . abs) (delta lst)

isSafe :: [Int] -> Bool
-- isSafe row = inRange row && oneWay row
isSafe = liftA2 (&&) inRange oneWay

{- FOURMOLU_DISABLE -}
{-

*Day02> xs = [1,2,3]
*Day02> zip xs (tail xs)
[(1,2),(2,3)]

*Day02> (zip <*> tail) [1,2,3]
[(1,2),(2,3)]

---

*Day02> zipWith (flip (-)) xs (tail xs)
[1,2,-1]
*Day02> xs = [1,2,4,3]

*Day02> zipWith (flip (-)) xs (tail xs)
[1,2,-1]
*Day02> (zipWith (flip (-)) <*> tail) [1,2,4,3]
[1,2,-1]

 -}
{- FOURMOLU_ENABLE -}
