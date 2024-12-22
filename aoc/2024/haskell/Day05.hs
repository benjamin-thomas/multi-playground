{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

module Day05 (main, splitManyA, splitManyB, splitManyC) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Either (lefts, rights)
import Data.List (unfoldr)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

rulePass :: Map Int [Int] -> [Int] -> Bool
rulePass rules = aux []
  where
    aux :: [Int] -> [Int] -> Bool
    aux seen remaining =
        case remaining of
            [] -> True
            (x : xs) ->
                let
                    isValid =
                        case Map.lookup x rules of
                            Nothing ->
                                x `notElem` seen
                            Just after ->
                                x `notElem` seen && all (`notElem` seen) after
                 in
                    isValid && aux (x : seen) xs

{-

First "naive" version, the may overflow the stack

>>> splitManyA ',' "1,2,3"
["1","2","3"]

>>> splitManyA ',' "1,"
["1"]

>>> splitManyA ',' "1"
["1"]

>>> splitManyA ',' ""
[]

 -}

splitManyA :: (Eq a) => a -> [a] -> [[a]]
splitManyA _ [] = []
splitManyA c xs = x : splitManyA c rest
  where
    (x, rest) = splitOnce c xs

{-

"Better" version, that won't overflow the stack

>>> splitManyB ',' "1,2,3"
["1","2","3"]

>>> splitManyB ',' "1,"
["1"]

>>> splitManyB ',' "1"
["1"]

>>> splitManyB ',' ""
[]

 -}
splitManyB :: (Eq a) => a -> [a] -> [[a]]
splitManyB c =
    aux []
  where
    aux acc [] = reverse acc
    aux acc xs =
        case splitOnce c xs of
            (x, rest) -> aux (x : acc) rest

{-

"Ultimate" version, as efficient as it could be.

>>> splitManyC ',' "1,2,3"
["1","2","3"]

>>> splitManyC ',' "1,"
["1"]

 -}
splitManyC :: (Eq a) => a -> [a] -> [[a]]
splitManyC c =
    unfoldr $ \case
        [] -> Nothing
        xs -> Just (splitOnce c xs)

{-

>>> splitOnce ',' "1,2,3"
("1","2,3")

>>> splitOnce ',' "1,"
("1","")

>>> splitOnce ',' "1"
("1","")

 -}

splitOnce :: (Eq a) => a -> [a] -> ([a], [a])
splitOnce c = second (drop 1) . break (== c)

-- mapM_ print $ Map.toList $ Data.List.sort <$> makeRules l
makeRules :: [String] -> Map Int [Int]
makeRules =
    foldr
        ( \line acc -> do
            let (k, v) = bimap read read $ splitOnce '|' line :: (Int, Int)
             in Map.alter (\xs -> Just (v : fromMaybe [] xs)) k acc
        )
        Map.empty

makeUpdates :: [String] -> [[Int]]
makeUpdates lst =
    fmap read . splitManyC ',' <$> lst

middle :: [a] -> a
middle lst = lst !! (length lst `div` 2)

applyRules :: (Map Int [Int], [[Int]]) -> [Either [Int] [Int]]
applyRules (rules, updates) =
    fmap
        ( \update ->
            if rulePass rules update
                then
                    Right update
                else
                    Left update
        )
        updates

ruleToNum :: Either [Int] [Int] -> Either [Int] Int
ruleToNum =
    \case
        Right xs -> Right $ middle xs
        Left xs -> Left xs

answer1 :: String -> Int
answer1 input =
    let (rules, updates) = bimap makeRules makeUpdates $ splitOnce [] $ lines input
     in sum $ rights (ruleToNum <$> applyRules (rules, updates))

reorder :: Map Int [Int] -> [Int] -> [Int]
reorder rules = aux []
  where
    aux :: [Int] -> [Int] -> [Int]
    aux left [] = left
    aux left (x : remaining) =
        case Map.lookup x rules of
            Nothing ->
                aux (x : left) remaining
            Just rule ->
                ( if List.any (`elem` remaining) rule
                    then
                        aux left (remaining ++ [x])
                    else
                        aux (x : left) remaining
                )

answer2 :: String -> Int
answer2 input =
    let (rules, updates) = bimap makeRules makeUpdates $ splitOnce [] $ lines input
        bad = lefts (applyRules (rules, updates))
     in sum $ middle . reorder rules <$> bad

-- ghcid -T :main ./Day05.hs
main :: IO ()
main = do
    example <- readFile "../_inputs/05.example"
    input <- readFile "../_inputs/05.txt"
    --
    putStr "Answer1 (example): " >> print (answer1 example) -- 143
    putStr "Answer1          : " >> print (answer1 input) -- 7307
    --
    putStrLn "---"
    --
    putStr "Answer2 (example): " >> print (answer2 example) -- 123
    putStr "Answer2          : " >> print (answer2 input) -- 4713
