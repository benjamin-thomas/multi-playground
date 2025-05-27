{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Main (main) where

import Control.Monad (replicateM)
import Text.Read (readMaybe)

read' :: Read a => String -> a
read' x = maybe (error $ "bad data: " <> x) id $ readMaybe x

main :: IO ()
main = do
    cnt <- read' <$> getLine :: IO Int
    nums <- replicateM cnt $ do
        fmap read' . words <$> getLine :: IO [Int]
    let answer = length . filter (>= 2) $ sum <$> nums
    print answer