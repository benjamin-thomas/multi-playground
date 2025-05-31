{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use replicate" #-}
module Main (main) where

{-

runghc ./Main.hs < ./input.txt

 -}

f :: Int -> [Int] -> [Int]
f n = concatMap (take n . repeat)

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main =
       getContents
              >>= mapM_ print . (\(n : xs) -> f n xs) . map read . words