{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use evalState" #-}
import Control.Monad.State (State, evalState, get, put, runState)

{-
  ghci> :cmd return $ unlines [":reload", "main"]
 -}

type Random a = State Int a -- this type is syntactic sugar, not really important

genRand :: Int -> Int
genRand seed =
    let (a, c, m) = (1103515245, 12345, 2 ^ 31)
     in (a * seed + c) `mod` m

{- | Generates a random number!

>>> fst $ runState randNum 0
12345

>>> fst $ runState randNum 12345
1406932606

>>> fst $ runState randNum 1406932606
654583775
-}
randNum :: Random Int
randNum = do
    seed <- get
    let newSeed = genRand seed
    put newSeed
    return newSeed

{- | Generates a random tuple of 3 items!


>>> fst $ runState randTup3 0
(12345,1406932606,654583775)
-}
randTup3 :: Random (Int, Int, Int)
randTup3 = do
    a <- randNum
    b <- randNum
    c <- randNum
    return (a, b, c)

main :: IO ()
main = do
    let seed = 0
        tupA = fst $ runState randTup3 seed
        tupB = evalState randTup3 seed
    print (tupA, tupB)
