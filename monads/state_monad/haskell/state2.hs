import Control.Monad (foldM)
import System.Random (randomRIO)

-- Tracking of state without the State monad

random :: Int -> Int
random seed =
    let (a, c, m) = (1103515245, 12345, 2 ^ 31)
     in (a * seed + c) `mod` m

{- | Manual state threading. It's error prone because we can easily swap a value (a,b,c)
>>> main1
1449466924
-}
main1 :: Int
main1 =
    let a = random 0
        b = random a
        c = random b
        d = random c
     in d

{- | Since `random` is pure, we don't actually need a Monad context


>>> main2
1449466924
-}
main2 :: Int
main2 = foldl (\acc f -> f acc) 0 [random, random, random, random]

{- | If I were using a Monad, I could also use `foldM`

>>> main3
1449466924

This one (main4) is random!
>>> main4
2092812124
-}
randomM :: (Monad m) => Int -> m Int
randomM seed = do
    return $ random seed

main3 :: IO Int
main3 = foldM (\acc f -> f acc) 0 [randomM, randomM, randomM, randomM]

main4 :: IO Int
main4 = do
    seed :: Int <- randomRIO (minBound, maxBound)
    foldM (\acc f -> f acc) seed [randomM, randomM, randomM, randomM]
