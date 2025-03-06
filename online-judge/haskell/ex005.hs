{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use uncurry" #-}

import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

{-

https://oj.moonbitlang.com/problems/202412-005-finding-the-largest-full-house

8:3
6:3
5:2

[8,3][6,2] => 36
[8,3][5,2] => 34
[6,3][8,2] => 34
[6,3][5,2] => 28

---

test {
  inspect!(solution(34, [6, 6, 6, 8, 8, 8, 5, 5, 1]), content="(8, 5)")
  inspect!(solution(37, [9, 9, 9, 9, 6, 6, 6, 6, 13]), content="(6, 9)")
  inspect!(solution(40, [1, 11, 13, 12, 7, 8, 11, 5, 6]), content="(0, 0)")
}

>>> solution 34 [6, 6, 6, 8, 8, 8, 5, 5, 1]
(8,5)

>>> solution 37 [9, 9, 9, 9, 6, 6, 6, 6, 13]
(6,9)

>>> solution 40 [1, 11, 13, 12, 7, 8, 11, 5, 6]
(0,0)

 -}
solution :: Int -> [Int] -> (Int, Int)
solution max cards =
    let
        -- sorted by "strength"
        -- combs =
        --     [ (8, 6)
        --     , (8, 5)
        --     , (6, 8)
        --     , (6, 5)
        --     ]
        combs = makeCombs cards

        step acc (a, b) = case acc of
            Just _ -> acc
            Nothing ->
                if (a * 3 + b * 2) > max
                    then
                        Nothing
                    else
                        Just (a, b)
        found = foldl step Nothing combs
     in
        fromMaybe (0, 0) found

{-

Cards that can make a "full house"

>>> candidates [6, 6, 6, 8, 8, 8, 5, 5, 1]
fromList [(5,2),(6,3),(8,3)]

 -}
candidates :: [Int] -> Map Int Int
candidates =
    let step n acc = Map.insertWith (+) n 1 acc
     in Map.filter (>= 2) . foldr step Map.empty

{-

>>> makeCombs [6, 6, 6, 8, 8, 8, 5, 5, 1]
[(8,6),(8,5),(6,8),(6,5)]

36 34 34

 -}

makeCombs :: [Int] -> [(Int, Int)]
makeCombs cards =
    let cs = candidates cards
        (as, bs) =
            Map.partition
                (> 2)
                (candidates cards)

        combs :: [(Int, Int)]
        combs = [(a, b) | a <- Map.keys as, b <- Map.keys bs <> Map.keys as, a /= b]
     in sortBy
            ( \(a1, b1) (a2, b2) ->
                case compare (a2 * 3 + b2 * 2) (a1 * 3 + b1 * 2) of
                    EQ -> compare a2 a1
                    x -> x
            )
            combs
