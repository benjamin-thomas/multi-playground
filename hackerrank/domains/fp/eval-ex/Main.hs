{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad (forM_)

{-

>>> fact <$> [0..7]
[1,1,2,6,24,120,720,5040]

>>> fact . realToFrac <$> [0..7]
[1.0,1.0,2.0,6.0,24.0,120.0,720.0,5040.0]

 -}
-- fact :: (Eq n, Num n) => n -> n
-- fact :: Double -> Double
fact :: (Eq n, Num n) => n -> n
fact 0 = 1
fact n = n * fact (n -1)

{-

Cool way to round float/double values
>>> (fromIntegral . truncate $ (1/3) * 10^4) / 10^4
0.3333

>>> roundTo' 4 (1/3)
0.3333

>>> roundTo' 4 143.68945656966488
143.6895

 -}
roundTo' :: (RealFrac r, Fractional f) => Int -> r -> f
roundTo' x n = (fromInteger . round $ n * 10 ^ x) / 10 ^ x

{-

>>> fst $ wip 5
143.689

 -}
compute :: (Eq n, Fractional n) => n -> (n, [(Integer, n, n)])
compute x =
    foldl
        ( \(tot, acc) n ->
            let a = x ^ n
                b = fact (fromIntegral n)
                curr = (a / b)
                sum' = (tot + curr)
             in ( sum'
                , ( n
                  , curr
                  , sum'
                  ) :
                  acc
                )
        )
        (0, [])
        [0 .. 9]

solve :: Double -> Double
solve n = roundTo' 4 . fst $ compute n

main :: IO ()
main = do
    n <- readLn :: IO Int

    forM_ [1 .. n] $ \_ -> do
        x <- readLn :: IO Double
        print $ solve x
