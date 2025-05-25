module Main where

import Control.Monad (replicateM_)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

{-

cat ./input.txt | runghc ./Main.hs

 -}

data AppErrors
    = BadFirstLine String
    | BadParams String
    | BadList String
    deriving (Show)

{-

>>> rotateRight 5 0 [1,2,3,4,5]
[1,2,3,4,5]

>>> rotateRight 5 1 [1,2,3,4,5]
[5,1,2,3,4]

>>> rotateRight 5 2 [1,2,3,4,5]
[4,5,1,2,3]

 -}

rotateLeft :: Int -> Int -> [a] -> [a]
rotateLeft len by = take len . drop by . cycle

rotateRight :: Int -> Int -> [a] -> [a]
rotateRight len by = reverse . rotateLeft len by . reverse

runLine :: ExceptT AppErrors IO ()
runLine = do
    line <- liftIO getLine
    case words line of
        [n, k] -> do
            case (readMaybe n, readMaybe k) of
                (Just n', Just k') -> do
                    lst <- words <$> liftIO getLine
                    case traverse readMaybe lst :: Maybe [Int] of
                        Nothing ->
                            throwError $ BadList line
                        Just lst2 -> do
                            liftIO $ putStrLn $ unwords $ map show $ rotateRight n' k' lst2
                _ -> do
                    throwError $ BadParams line
        _ -> do
            throwError $ BadFirstLine line

run :: ExceptT AppErrors IO ()
run = do
    times <- do
        line <- liftIO getLine
        maybe
            (throwError $ BadFirstLine line)
            pure
            (readMaybe line)

    replicateM_
        times
        runLine

main :: IO ()
main = do
    result <- runExceptT run
    case result of
        Left err -> do
            putStrLn "Fatal error!"
            putStr ">> "
            print err
        Right _ ->
            pure ()
