{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

module Day05 (main, splitManyA, splitManyB, splitManyC) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)

rulePass :: [Int] -> Map Int [Int] -> [Int] -> Bool
rulePass seen rules remaining =
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
                isValid && rulePass (x : seen) rules xs

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

-- rules :: Map Int [Int]
-- rules =
--     Map.fromList
--         [ (29, [13])
--         , (47, [13, 29, 53, 61])
--         , (53, [13, 29])
--         , (61, [13, 29, 53])
--         , (75, [13, 29, 47, 53, 61])
--         , (97, [13, 29, 47, 53, 61, 75])
--         ]

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

answer1 :: [Char] -> [Int]
answer1 input =
    let (rules, updates) = bimap makeRules makeUpdates $ splitOnce [] $ lines input
        rulesApplied =
            fmap
                ( \update ->
                    if rulePass [] rules update
                        then
                            Just update
                        else Nothing
                )
                updates
        middle lst = lst !! (length lst `div` 2)
     in middle <$> catMaybes rulesApplied

-- ghcid -T :main ./Day05.hs
main :: IO ()
main = do
    exampleInput <- readFile "../_inputs/05.example"
    input <- readFile "../_inputs/05.txt"
    putStr "Answer1 (example): " >> print (sum $ answer1 exampleInput) -- 143
    putStr "Answer1          : " >> print (sum $ answer1 input) -- 7307
