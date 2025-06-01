{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.List (transpose, unfoldr)

{-

Appart from the first row, think of a triangle row as:

1 <MIDDLE> 1

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

From any row, we can compute the next "middle" like so:

Prelude> uncurry (+) <$> zip [1,1] [1]
[2]
Prelude> uncurry (+) <$> zip [1,2,1] [2,1]
[3,3]
Prelude> uncurry (+) <$> zip [1,3,3,1] [3,3,1]
[4,6,4]
Prelude> uncurry (+) <$> zip [1,4,6,4,1] [4,6,4,1]
[5,10,10,5]

              1
            1   1
          1   2   1
        1   3   3   1
      1   4   6   4   1
    1   5  10  10   5   1
  1   6  15  20  15   6   1
1   7  21  35  35  21   7   1

Turns out this is also touches a concept called "ZipList"

Prelude Control.Applicative> (,) <$> ZipList [1,4,6,4,1] <*> ZipList [4,6,4,1]
ZipList {getZipList = [(1,4),(4,6),(6,4),(4,1)]}
Prelude Control.Applicative> (+) <$> ZipList [1,4,6,4,1] <*> ZipList [4,6,4,1]
ZipList {getZipList = [5,10,10,5]}

Which is also slightly related to the concept of "transpose" (which doesn't "truncate")

Prelude Data.List> transpose [[1,4,6,4,1], [4,6,4,1]]
[[1,4],[4,6],[6,4],[4,1],[1]]
Prelude Data.List> sum <$> transpose [[1,4,6,4,1], [4,6,4,1]]
[5,10,10,5,1]

>>> nextRowTranspose [1]
[1,1]
>>> nextRowTranspose [1,1]
[1,2,1]
>>> nextRowTranspose [1,2,1]
[1,3,3,1]
>>> nextRowTranspose [1,3,3,1]
[1,4,6,4,1]
>>> nextRowTranspose $ nextRowTranspose $ nextRowTranspose [1,1]
[1,4,6,4,1]

This is just "iterate"

>>> take 5 $ iterate nextRowTranspose [1]
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]

 -}

nextRowTranspose :: Num a => [a] -> [a]
nextRowTranspose xs = 1 : (sum <$> transpose [xs, tail xs])

nextRow :: [Int] -> [Int]
nextRow row = [1] ++ middleElements ++ [1]
 where
  pairs = zip row (tail row)
  middleElements = [a + b | (a, b) <- pairs]

{-

Here's how to make a sliding window of 3

>>> zip3 [1,2,3,4,5] [2,3,4,5] [3,4,5]
[(1,2,3),(2,3,4),(3,4,5)]

(\[a,b,c] -> zip3 a b c) $ (\n -> [n..5]) <$> [1..3]

Data.List Control.Applicative> (,) <$> ZipList [1..5] <*> ZipList [2..5]
ZipList {getZipList = [(1,2),(2,3),(3,4),(4,5)]}

*Data.List Control.Applicative> (,,) <$> ZipList [1..5] <*> ZipList [2..5] <*> ZipList [3..5]
ZipList {getZipList = [(1,2,3),(2,3,4),(3,4,5)]}

Holly shit...

> take 3 (tails [1..5])
[[1,2,3,4,5],[2,3,4,5],[3,4,5]]

> ZipList <$> take 3 (tails [1..5])
[ZipList {getZipList = [1,2,3,4,5]},ZipList {getZipList = [2,3,4,5]},ZipList {getZipList = [3,4,5]}]

> sequenceA $ ZipList <$> take 3 (tails [1..5])
ZipList {getZipList = [[1,2,3],[2,3,4],[3,4,5]]}

> foldr (zipWith (:)) (repeat []) $ take 3 $ tails [1..5]
[[1,2,3],[2,3,4],[3,4,5]]

> sequenceA [[1,2], [3,4]]
[[1,3],[1,4],[2,3],[2,4]]

> (,) <$> [1,2] <*> [3,4]
[(1,3),(1,4),(2,3),(2,4)]

---

In Elm:

> List.foldr (Maybe.map2 (::)) (Just []) [Just 1, Just 2, Just 3]
Just [1,2,3] : Maybe (List number)
> List.foldr (Maybe.map2 (::)) (Just []) [Just 1, Nothing, Just 3]
Nothing : Maybe (List number)

---

In Haskell:

> sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]

> sequenceA [Just 1, Nothing, Just 3]
Nothing

So... (sequenceA is traverse id)

> foldr (\x acc -> (:) <$> x <*> acc) (Just []) [Just 1, Just 2, Just 3]
Just [1,2,3]
> foldr (\x acc -> (:) <$> x <*> acc) (Just []) [Just 1, Nothing, Just 3]
Nothing

> foldr (liftA2 (:)) (Just []) [Just 1, Just 2, Just 3]
Just [1,2,3]
> foldr (liftA2 (:)) (Just []) [Just 1, Nothing, Just 3]
Nothing

> traverse id [Just 1, Just 2, Just 3]
Just [1,2,3]
> traverse id [Just 1, Nothing, Just 3]
Nothing

> traverse (fmap (*2)) [Just 1, Just 2, Just 3]
Just [2,4,6]
> traverse (fmap (*2)) [Just 1, Nothing, Just 3]
Nothing

> foldl (liftA2 $ flip (:)) (Just []) [Just 1, Just 2, Just 3]
Just [3,2,1]
> foldl (liftA2 $ flip (:)) (Just []) [Just 1, Nothing, Just 3]
Nothing

---

A very interesting quote found here: https://stackoverflow.com/questions/27726739/implementing-an-efficient-sliding-window-algorithm-in-haskell

> Zipping a list of lists is just a transposition, but unlike...

> (,) <$> ZipList [1,2,3] <*> ZipList [4,5,6]
ZipList {getZipList = [(1,4),(2,5),(3,6)]}
> (,,) <$> ZipList [1,4] <*> ZipList [2,5] <*> ZipList [3,6]
ZipList {getZipList = [(1,2,3),(4,5,6)]}

> transpose [[1,2,3],[4,5,6]]
[[1,4],[2,5],[3,6]]
> transpose [[1,4],[2,5],[3,6]]
[[1,2,3],[4,5,6]]

> (,) <$> ZipList [1,2,3] <*> ZipList [4,5]
ZipList {getZipList = [(1,4),(2,5)]}
> transpose [[1,2,3],[4,5]]
[[1,4],[2,5],[3]]

 -}

bonkers :: Int -> [[Int]]
bonkers n = take n $ iterate nextRow []

{-

>>> take 6 pascal_A
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

---

We could hypothetically continue building the tree from any row:

> take 3 (iterate nextRow [1,4,6,4,1])
[[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]
 -}
pascal_A :: [[Int]]
pascal_A = iterate nextRow [1]

{-

>>> take 6 pascal_B
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

 -}
pascal_B :: [[Int]]
pascal_B = unfoldr (\xs -> Just (xs, nextRow xs)) [1]

{-

Summing up:

>>> take 6 pascal_C
[[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]

 -}

pascal_C :: [[Int]]
pascal_C =
  let genRow row =
        let middle =
              let window2 = zip row (tail row)
               in uncurry (+) <$> window2
         in [1] ++ middle ++ [1]
   in unfoldr (\xs -> Just (xs, genRow xs)) [1]

main :: IO ()
main =
  mapM_ print $ take 6 pascal_C

--------------------------------------------------------------------------------
-- Below was a prior attempt to print the triangle, based on a math formula.
-- (got the formula on HackerRank)
-- Less intuitive and not as "elegant"...
--------------------------------------------------------------------------------

fact :: (Num n, Enum n) => n -> n
fact n = product [1 .. n]

at :: Int -> Int -> Int
at row col =
  let (dbl :: Double) = fromIntegral (fact row) / fromIntegral (fact col * fact (row - col))
   in truncate dbl

{-

>>> rowToString . (\n -> at n <$> [0 .. n]) <$> [0 .. 5]
["1","1 1","1 2 1","1 3 3 1","1 4 6 4 1","1 5 10 10 5 1"]

 -}
rowToString :: Show a => [a] -> String
rowToString row = unwords $ show <$> row
