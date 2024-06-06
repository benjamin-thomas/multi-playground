module Main (main) where

import Data.ByteString.Char8 qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)
import System.IO (Handle, IOMode (ReadMode), hGetLine, openFile)
import System.IO.Error (catchIOError)

{-
rg --files | entr -c bash -c 'time runghc -Wall ./main.hs ../test.txt'

ghc -o ./ccwc -O2 ./main.hs
rm *.o *.hi

NOTE: perf is not good
  - roughly 10 times slower than my OCaml version
  - 2 times slower than my Ruby version!
 -}

byteCount :: String -> Int
byteCount = B.length . TE.encodeUtf8 . T.pack

runeCount :: String -> Int
runeCount = T.length . T.pack

wordCount :: String -> Int
wordCount str =
    let x = T.words $ T.pack str
     in length x

data Counters = MkCounters
    { cBytes :: Int
    , cLines :: Int
    , cWords :: Int
    , cRunes :: Int
    }

handleLine :: Counters -> String -> Counters
handleLine acc line =
    MkCounters
        { cBytes = cBytes acc + byteCount line + 1
        , cLines = cLines acc + 1
        , cWords = cWords acc + wordCount line
        , cRunes = cRunes acc + runeCount line + 1
        }

handleFile :: Counters -> Handle -> IO Counters
handleFile acc handle = do
    result <- catchIOError (Right <$> hGetLine handle) (\_ -> return (Left ()))

    case result of
        Left () -> return acc
        Right line -> do
            let counters = handleLine acc line
            handleFile counters handle

showCounters :: FilePath -> Counters -> String
showCounters path counters =
    "file="
        ++ takeBaseName path
        ++ "  "
        ++ "bytes="
        ++ show (cBytes counters)
        ++ "  "
        ++ "lines="
        ++ show (cLines counters)
        ++ "  "
        ++ "words="
        ++ show (cWords counters)
        ++ "  "
        ++ "runes="
        ++ show (cRunes counters)

handleFiles :: [String] -> IO ()
handleFiles names = case names of
    [] -> return ()
    (x : xs) -> do
        file <- openFile x ReadMode
        counters <- handleFile (MkCounters 0 0 0 0) file
        putStrLn $ showCounters x counters
        handleFiles xs

main :: IO ()
main = do
    filepaths <- getArgs
    handleFiles filepaths
