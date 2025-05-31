module Main (main) where

f :: Int -> [Int] -> [Int]
f = aux []
  where
    aux acc n [] = reverse acc
    aux acc n (x : xs) =
        aux
            ( if x < n
                then x : acc
                else acc
            )
            n
            xs

{-

runghc ./Main.hs < ./input.txt

 -}

-- The Input/Output section. You do not need to change or modify this part
main :: IO ()
main = do
    n <- readLn :: IO Int
    inp <- getContents
    let numbers = map read (lines inp) :: [Int]
    putStrLn . unlines $ (map show . f n) numbers