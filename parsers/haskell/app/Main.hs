module Main where

import MyLib qualified (run)

main :: IO ()
main = do
  putStrLn "Testing some parsers..."
  MyLib.run
