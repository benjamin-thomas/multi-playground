module Main where

import Control.Monad (unless)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate)
import System.Environment.Blank (getArgs)
import System.IO (isEOF)

{-

echo ./Main.hs | entr -c bash -rc 'runghc -Wall ./Main.hs 16 < <(echo -e "ABC\ndef\nXY2Z\nFF")'

 -}

red :: String
red = "\x1b[31m"

reset :: String
reset = "\x1b[0m"

toBase10 :: Char -> Maybe Int
toBase10 c
    | isDigit c = Just $ fromEnum c - fromEnum '0'
    | isAsciiUpper c = Just $ fromEnum c - fromEnum 'A' + 10
    | isAsciiLower c = Just $ fromEnum c - fromEnum 'a' + 10
    | otherwise = Nothing

{- | Converts a string representation of a number to its base 10 representation. Supports converting numbers from bases 2-36.

>>> convertToBase10 16 "ABC"
([12,11,10],"")

>>> convertToBase10 16 "EFG"
([15,14],"G")

>>> convertToBase10 36 "XYZ"
([35,34,33],"")

>>> convertToBase10 37 "XYZ"
([],"ZYX")

>>> convertToBase10 2 "0123"
([1,0],"32")

>>> convertToBase10 1 "0123"
([],"3210")
-}
convertToBase10 :: Int -> String -> ([Int], [Char])
convertToBase10 base =
    foldl
        ( \(good, bad) c ->
            case toBase10 c of
                Nothing -> (good, c : bad)
                Just n ->
                    if n < 0 || n >= base || base > 36 || base < 2
                        then
                            (good, c : bad)
                        else
                            (n : good, bad)
        )
        ([], [])

{-

Horner's method states that:
    A×16² + B×16¹ + C×16⁰

Is equivalent to:
    C + 16×(B + 16×(A + 16×0))

I'm using foldr to handle the list which is already in reverse order.

While converting "0xABC":
>>> hornerRev 16 [12,11,10]
2748
-}
hornerRev :: Int -> [Int] -> Int
hornerRev base =
    foldr (\n acc -> n + base * acc) 0

handleLine :: Int -> [Char] -> IO ()
handleLine base line = do
    putStrLn $
        mconcat
            [ "["
            , intercalate "," $ map show line
            , "]"
            ]

    let (good, bad) = convertToBase10 base line

    case (good, bad) of
        (vals, []) -> do
            print (reverse vals) -- just for clarity, for the user
            putStrLn $ " -> " <> show (hornerRev base vals)
        (_, chars) ->
            putStrLn $
                mconcat
                    [ red
                    , " => Bad chars: " <> chars
                    , reset
                    ]
    putStrLn ""

mainLoop :: Int -> IO ()
mainLoop base = do
    eof <- isEOF
    unless eof loop
  where
    loop :: IO ()
    loop = do
        line <- getLine
        handleLine base line
        mainLoop base

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n] -> do
            let base = read n
            putStrLn $ "Base is: " <> show base
            putStrLn "---\n"
            mainLoop base
            putStrLn "Done!"
        _ ->
            putStrLn "Usage ./Main.hs BASE"
