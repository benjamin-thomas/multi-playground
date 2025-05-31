{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

fact :: (Ord n, Num n) => n -> n
fact n
    | n < 0 = error "[fact] bad data: negative input"
    | n == 0 = 1
    | otherwise = n * fact (n -1)

{-

Computes the Pascal Triangle value for a given row/col position.

>>> at 4 <$> [0..4]
[1,4,6,4,1]

 -}
at :: Int -> Int -> Int
at row col =
    let (dbl :: Double) = fromIntegral (fact row) / fromIntegral (fact col * fact (row - col))
     in truncate dbl

{-

>>> rowToString [1,2,1]
"1 2 1"

 -}
rowToString :: Show a => [a] -> String
rowToString row = unwords $ show <$> row

main :: IO ()
main =
    getLine >>= \str ->
        mapM_ putStrLn $
            rowToString . (\n -> at n <$> [0 .. n]) <$> [0 .. read str - 1]
