module Main (main) where

import Control.Monad (guard)
import Data.List (sort, subsequences)

example :: String
example = "QAQAQYSYIOIWIN" -- spell: disable-line

{-

What do I want? I want:

1 2 3
1 2 4
1 2 5
1 3 4
1 3 5
1 4 5
2 3 4
2 3 5
3 4 5

This is N choose K, N! / K! * (n-k)!

> 5! / (3! * (5-3)!)

  factorial(5) / (factorial(3) × factorial(5 − 3)) = 10

So I want a **combination**

 -}

combs :: Int -> [a] -> [[a]]
combs n xs =
  filter
    (\xs -> n == length xs)
    (subsequences xs)

{- | Returns the combinations of [xs], as in N choose 3.

This is much more performant than dynamically building the hypothetical K value.
Although, there are probably ways to compute the combinations with a dynamic K value,
this is non-trivial so it's better to stick to this simplest approach if possible.
-}
combs3 :: [a] -> [[a]]
combs3 xs = do
  (i, x) <- zip [0 ..] xs
  (j, y) <- zip [0 ..] xs
  (k, z) <- zip [0 ..] xs
  guard (i < j && j < k)
  pure [x, y, z]

{-

================================================================================
EXPLANATION

Just for clarity, it's easier to think of list of list as [] : [], rather than [[]]

*Main> (1:) <$> [] : []
[[1]]
*Main> [1] : []
[[1]]

================================================================================

*Main> subsets [1,2,3]
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- Simulating the tracing of subsets [1,2,3]:

. x = 1, xs = [2,3]
. First, calculate left = subsets [2,3]

  .. x = 2, xs = [3]
  .. First, calculate left = subsets [3]

    ... x = 3, xs = []
    ... First, calculate left = subsets [] = [[]] (base case)
    ... result
    ... = left ++ map (3:) left
    ... = [[]] ++ map (3:) [[]]
    ... = [[],[3]]

  .. Now left = [[],[3]]
  .. result
  .. = left ++ map (2:) left
  .. = [[],[3]] ++ map (2:) [[],[3]]
  .. = [[],[3],[2],[2,3]]

. Now left = [[],[3],[2],[2,3]]
. result
. = left ++ map (1:) left
  = [[],[3],[2],[2,3]] ++ map (1:) [[],[3],[2],[2,3]]
  = [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

*Main> subsets [2,3]
[[],[3],[2],[2,3]]

 -}
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) =
  let left = subsets xs
   in left ++ map (x :) left

{-

>>> sort $ map sort $ subsets [1,2,3]
[[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]

>>> sort $ map sort $ subsets'  [1,2,3]
[[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]

>>> subsets' [1,2,3]
[[],[1],[2],[2,1],[3],[3,1],[3,2],[3,2,1]]

================================================================================

Simulating the tracing of subsets' [1,2,3]:

*   acc = [[]], x = 1
.   acc = [[]] ++ map (1:) [[]]
.   acc = [[],[1]]
**  acc = [[],[1]], x = 2
..  acc = [[],[1]] ++ map (2:) [[],[1]]
..  acc = [[],[1],[2],[2,1]]
*** acc = [[],[1],[2],[2,1]], x = 3
... acc = [[],[1],[2],[2,1]] ++ map (3:) [[],[1],[2],[2,1]]
... res = [[],[1],[2],[2,1],[3],[3,1],[3,2],[3,2,1]]

 -}
subsets' :: [a] -> [[a]]
subsets' = foldl step [[]]
 where
  step acc x = acc ++ map (x :) acc

combs' :: Int -> [a] -> [[a]]
combs' n xs =
  filter
    (\xs -> n == length xs)
    (subsets' xs)

solution1 :: String -> Int
solution1 = length . filter (== "QAQ") . combs' 3

solution2 :: String -> Int
solution2 = length . filter (== "QAQ") . combs 3

solution3 :: String -> Int
solution3 = length . filter (== "QAQ") . combs3

-- I'll have to study it this Claude suggestion (perf is as good)
-- solution4 :: String -> Int
-- solution4 = length . filter (== "QAQ") . combsK 3

-- combsK :: Int -> [a] -> [[a]]
-- combsK k xs = go k (zip [0 ..] xs)
--  where
--   go 1 ixs = [[x] | (_, x) <- ixs]
--   go n ixs = do
--     (i, x) <- ixs
--     ys <- go (n -1) [(j, y) | (j, y) <- ixs, j > i]
--     return (x : ys)

{-

This one is quite slow! (~3s)
spell: disable-next-line
time runghc ./Main.hs < <(echo "IAQVAQZLQBQVQFTQQQADAQJA")

---

This one is just too slow! (very much worse)
spell: disable-next-line
time runghc ./Main.hs < <(echo "QQAAQASGAYAAAAKAKAQIQEAQAIAAIAQQQQQ")

To compile with optimal performance, run:

ghc -O2 Main.hs

 -}
main :: IO ()
main =
  -- print . solution1 =<< getLine -- 9s
  -- print . solution2 =<< getLine -- 3s
  print . solution3 =<< getLine -- 150ms
