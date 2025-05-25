module Main2 where

import Control.Monad (replicateM_)

{-

Variation without error handling (similar in size to the Ruby script)

cat ./input.txt | runghc ./Main2.hs

 -}

rotateRight :: Int -> Int -> [a] -> [a]
rotateRight len by xs =
    reverse . take len . drop by . cycle $ reverse xs

readNums :: String -> [Int]
readNums = fmap read . words

handleLine :: IO ()
handleLine = do
    [n, k] <- readNums <$> getLine
    lst <- readNums <$> getLine
    putStrLn $
        unwords
            (show <$> rotateRight n k lst)

main :: IO ()
main = do
    times <- read <$> getLine
    replicateM_ times handleLine