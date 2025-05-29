{-# OPTIONS_GHC -Wall #-}

module Factorize where

divisibleBy :: Int -> Int -> Bool
divisibleBy divisor rest = 0 == mod rest divisor

sieve :: [Int] -> [Int]
sieve [] = []
sieve (divisor : divisors) =
  divisor :
  sieve
    ( filter
        (not . divisibleBy divisor)
        divisors
    )

{-

>>> take 10 primes
[2,3,5,7,11,13,17,19,23,29]

 -}
primes :: [Int]
primes = sieve [2 ..]

{-

*Wip Data.List> mapM_ print $ take 10 $ (,) <*> factorize <$> [9192..]
(9192,[383,3,2,2,2])         => 383 × 3 × 2³    = 9192
(9193,[317,29])              => 317 × 29        = 9193
(9194,[4597,2])              => 4597 × 2        = 9194
(9195,[613,5,3])             => 613 × 5 × 3     = 9195
(9196,[19,11,11,2,2])        => 19 × 11² × 2²   = 9196
(9197,[541,17])              => 541 × 17        = 9197
(9198,[73,7,3,3,2])          => 73 × 7 × 3² × 2 = 9198
(9199,[9199])                => PRIME!          = PRIME!
(9200,[23,5,5,2,2,2,2])      => 23 × 5² × 2⁴    = 9200
(9201,[3067,3])              => 3067 × 3        = 9201

>>> take 3 $ (,) <*> factorize <$> [100..]
[(100,[5,5,2,2]),(101,[101]),(102,[17,3,2])]

 -}
factorize :: Int -> [Int]
factorize n =
  let candidates :: [Int]
      candidates = takeWhile (<= n) primes

      step :: (Int, [Int]) -> Int -> (Int, [Int])
      step (rest, list) divisor =
        if rest `mod` divisor == 0
          then
            step
              ( rest `div` divisor
              , divisor : list
              )
              divisor
          else
            ( rest
            , list
            )
   in snd $
        foldl
          step
          (n, [])
          candidates
