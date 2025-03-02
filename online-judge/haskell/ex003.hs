import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- https://oj.moonbitlang.com/problems/202412-003-number-string-formatting

{-

:cmd return $ unlines [":reload", "wip"]
ghcid ./ex003.hs -T :main

 -}

insertSeparator :: Int -> String -> String
insertSeparator offset str =
    snd $
        foldr
            ( \c (n, acc) ->
                ( n + 1
                , c
                    : if n > offset && n `mod` 3 == (offset `mod` 3)
                        then ',' : acc
                        else acc
                )
            )
            (0, [])
            str

{-

>>> solution "1294512.12412"
"1,294,512.12412"

>>> solution "123456789.99"
"123,456,789.99"

>>> solution "987654321"
"987,654,321"

>>> solution "0000123456789.99"
"123,456,789.99"

>>> solution "0.123"
"0.123"

>>> solution ""
""

 -}
solution :: String -> String
solution str =
    let str2 = stripZeroes str
     in case str2 of
            '.' : _ -> str
            _ ->
                insertSeparator (findOffset str2) str2

{-

>>> findOffset "123456789"
0

>>> findOffset "1234.5"
2

>>> findOffset "1234.56"
3

 -}
findOffset :: String -> Int
findOffset =
    snd
        . foldr
            ( \c (n, foundIdx) ->
                ( n + 1
                , if foundIdx == 0 && c == '.'
                    then n + 1
                    else foundIdx
                )
            )
            (0, 0)

{-

>>> stripZeroes "0000123xxx"
"123xxx"

>>> stripZeroes "123yyy"
"123yyy"

 -}
stripZeroes :: String -> String
stripZeroes =
    reverse
        . foldl
            ( \acc c ->
                if c == '0'
                    then acc
                    else c : acc
            )
            []

main :: IO ()
main = do
    putStrLn $ insertSeparator 0 "00000000" -- 3,6,9 [offset=0]
    putStrLn $ solution "123456789"
    putStrLn "---"
    putStrLn $ insertSeparator 2 "222222222222.0" -- 5,8,11 [offset=2]
    putStrLn $ solution "1234.5"
    putStrLn "---"
    putStrLn $ insertSeparator 3 "3333333333.56" -- 6,9,12 [offset=3]
    putStrLn $ solution "1234.56"
    putStrLn "---"
    putStrLn $ insertSeparator 4 "444444444.789" -- 6,9,12 [offset=4]
    putStrLn $ solution "11334234.5678999"
