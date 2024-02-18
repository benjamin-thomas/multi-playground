module Sorting where

-- doctest ./sorting.hs

{- | Merge 2 sorted lists.

>>> merge [] []
[]

>>> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
-}
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

{- | Split a list into 2 halves whose length differs by at most one.

>>> halve []
([],[])

>>> halve [1]
([1],[])

>>> halve [1,2]
([1],[2])

>>> halve [1,2,3]
([3,1],[2])

>>> halve [1,2,3,4]
([3,1],[4,2])

>>> halve [1,2,3,4,5]
([5,3,1],[4,2])

>>> halve [1,2,3,4,5,6]
([5,3,1],[6,4,2])
-}
halve :: [a] -> ([a], [a])
halve =
    aux True ([], [])
  where
    aux flip (l, r) lst =
        case lst of
            [] -> (l, r)
            (x : xs) ->
                if flip
                    then aux (not flip) (x : l, r) xs
                    else aux (not flip) (l, x : r) xs

{- | Now use merge and halve to implement merge sort.

>>> mSort [1,4,3,6,5,4,8,2,3,1,9]
[1,1,2,3,3,4,4,5,6,8,9]
-}
mSort :: (Ord a) => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort lst =
    let (l, r) = halve lst
     in merge (mSort l) (mSort r)