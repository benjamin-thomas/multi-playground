{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

read' :: (Read a) => String -> a
read' = fromMaybe (error "bad data") . readMaybe

{-

runghc ./Main.hs < <(echo 8)

 -}

main :: IO ()
main = do
    n <- read' <$> getLine :: IO Int
    putStrLn $
        if even n && n > 2
            then "YES"
            else "NO"