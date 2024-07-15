module Main (main) where

import Data.Maybe (fromMaybe)
import MyLib qualified
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Text.Read (readMaybe)

envPortOr :: Int -> IO Int
envPortOr defaultPort = do
  mEnv <- lookupEnv "PORT"
  let mPort = readMaybe =<< mEnv
  return $ fromMaybe defaultPort mPort

main :: IO ()
main = do
  port <- envPortOr 8080
  printf "Starting app on port %d\n" port
  MyLib.someFunc
  printf "Some value is: %d\n" MyLib.someValue
  MyLib.startApp port
