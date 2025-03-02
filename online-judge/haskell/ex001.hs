{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Bits (xor)

-- https://oj.moonbitlang.com/problems/202412-001-find-the-unique-number

{-
>>> solution([1, 1, 2, 2, 3, 3, 4, 5, 5])
4

>>> solution([0, 1, 0, 1, 2])
2

>>> solution([7, 3, 3, 7, 10])
10
 -}
solution :: [Int] -> Int
solution = foldr (flip xor) 0
