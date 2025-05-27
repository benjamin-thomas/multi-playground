{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

read' :: (Read a) => String -> a
read' = fromMaybe (error "bad data") . readMaybe

{-

runghc ./Main.hs < input.txt

 -}

shorten :: String -> String
shorten str =
    let len = length str
     in if len <= 10
            then str
            else head str : show (len - 2) ++ [last str]

main :: IO ()
main = do
    cnt <- read' <$> getLine
    replicateM_ cnt $ do
        word <- getLine
        putStrLn $ shorten word
