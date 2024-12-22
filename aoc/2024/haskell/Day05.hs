{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

module Day05 (main) where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Bool (bool)
import Data.Either (lefts, rights)
import Data.List (unfoldr)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, notMember)
import Data.Set qualified as Set

rulePass :: Map Int (Set Int) -> [Int] -> Bool
rulePass rules = aux Set.empty
  where
    aux :: Set Int -> [Int] -> Bool
    aux seen remaining =
        case remaining of
            [] -> True
            (x : xs) ->
                let
                    notBefore = x `notMember` seen

                    notAfter =
                        case Map.lookup x rules of
                            Nothing ->
                                True
                            Just after ->
                                all (`notMember` seen) after
                 in
                    notBefore && notAfter && aux (Set.insert x seen) xs

{-

>>> splitMany ',' "1,2,3"
["1","2","3"]

>>> splitMany ',' "1,"
["1"]

 -}
splitMany :: (Eq a) => a -> [a] -> [[a]]
splitMany c =
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

makeRules :: [String] -> Map Int (Set Int)
makeRules =
    foldr
        ( \line dict ->
            let (key :: Int, val :: Int) = bimap read read $ splitOnce '|' line
             in Map.alter
                    ( \case
                        Nothing -> Just (Set.singleton val)
                        Just xs -> Just (Set.insert val xs)
                    )
                    key
                    dict
        )
        Map.empty

makeUpdates :: [String] -> [[Int]]
makeUpdates lst =
    fmap read . splitMany ',' <$> lst

middle :: [a] -> a
middle lst = lst !! (length lst `div` 2)

{-
>>> fmap (bool <$> Left <*> Right <*> even) [1,2,3]
[Left 1,Right 2,Left 3]

>>> bool <$> Left <*> Right <*> even <$> [1,2,3]
[Left 1,Right 2,Left 3]

 -}
applyRules :: (Map Int (Set Int), [[Int]]) -> [Either [Int] [Int]]
applyRules (rules, updates) =
    bool <$> Left <*> Right <*> rulePass rules <$> updates

answer1 :: String -> Int
answer1 input =
    let (rules, updates) = bimap makeRules makeUpdates $ splitOnce [] $ lines input
        good = rights $ applyRules (rules, updates)
     in sum $ middle <$> good

reorder :: Map Int (Set Int) -> [Int] -> [Int]
reorder rules = aux []
  where
    aux :: [Int] -> [Int] -> [Int]
    aux acc [] = acc
    aux acc (x : rest) =
        case Map.lookup x rules of
            Nothing ->
                aux (x : acc) rest
            Just rule ->
                if List.any (`elem` rest) rule
                    then
                        aux acc (rest ++ [x])
                    else
                        aux (x : acc) rest

answer2 :: String -> Int
answer2 input =
    let (rules, updates) = bimap makeRules makeUpdates $ splitOnce [] $ lines input
        bad = lefts $ applyRules (rules, updates)
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
