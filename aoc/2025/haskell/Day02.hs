{-# OPTIONS_GHC -Wall #-}

{-

Map over IO (twice), then map into Right.

λ> readFile "../inputs/Day02.example" <&> parse items "" <&> fmap (take 1)
Right [(11,22)]
λ> readFile "../inputs/Day02.example" <&> parse items "" <&> fmap length
Right 11

Then we can build the rest of the "pipeline"
λ> readFile "../inputs/Day02.example" <&> parse items "" <&> fmap (concatMap findBadIds2 >>> map read >>> sum)
Right 4174379265
-}

import Text.ParserCombinators.Parsec

-- TEST HELPERS

red, green, reset :: String
red   = "\ESC[31m"
green = "\ESC[32m"
reset = "\ESC[0m"

test :: (Eq a, Show a) => String -> a -> a -> IO ()
test name l r = do
    putStr name
    if l == r
        then
            putStrLn $ "  " ++ green ++ "✓ " ++ show r ++ reset
        else do
            putStrLn $ "  " ++ red ++ "✗" ++ reset
            putStrLn $ "  " ++ red ++ "left : " ++ show l ++ reset
            putStrLn $ "  " ++ red ++ "right: " ++ show r ++ reset

-- PARSING

int :: Parser Integer
int = read <$> many1 digit

range :: Parser (Integer, Integer)
range = (,) <$> int <* char '-' <*> int

ranges :: Parser [(Integer, Integer)]
ranges = range `sepBy` char ','

parseInput :: String -> Either ParseError [(Integer, Integer)]
parseInput = parse ranges ""

-- PART 1

isBadId1 :: String -> Bool
isBadId1 str =
    even len && take mid str == drop mid str
  where
    len = length str
    mid = len `div` 2

filterBadIds1 :: (Integer, Integer) -> [String]
filterBadIds1 (lo, hi) =
    filter isBadId1 $ map show [lo..hi]

solve1 :: [(Integer, Integer)] -> Integer
solve1 = sum . map read . concatMap filterBadIds1

-- PART 2

replicate' :: String -> Int -> String
replicate' pat n = concat $ replicate n pat

isRepeating :: String -> String -> Int -> Bool
isRepeating str pat n = str == replicate' pat n

hasRepeatPattern :: Int -> String -> Int -> Bool
hasRepeatPattern len str n
    | n > len   = False
    | otherwise =
        isRepeating str (take (len `div` n) str) n
            || hasRepeatPattern len str (n + 1)

isBadId2 :: String -> Bool
isBadId2 str
    | even len  = hasRepeatPattern len str 2
    | otherwise = hasRepeatPattern len str 3
  where
    len = length str

filterBadIds2 :: (Integer, Integer) -> [String]
filterBadIds2 (lo, hi) =
    filter isBadId2 $ map show [lo..hi]

solve2 :: [(Integer, Integer)] -> Integer
solve2 = sum . map read . concatMap filterBadIds2

-- MAIN

main :: IO ()
main = do
    putStrLn "=== Parsing ==="
    test "parse range \"11-22\"        " (Right (11, 22))
                                         (parse range "" "11-22")
    test "parse ranges \"11-22,95-115\"" (Right [(11, 22), (95, 115)])
                                         (parse ranges "" "11-22,95-115")

    putStrLn "\n=== Part 1 ==="
    test "isBadId1 \"1212\"" True  (isBadId1 "1212")
    test "isBadId1 \"1234\"" False (isBadId1 "1234")
    test "isBadId1 \"123\" " False (isBadId1 "123")
    test "filterBadIds1 (11,22)" ["11", "22"]    (filterBadIds1 (11, 22))

    putStrLn "\n=== Part 2 ==="
    test "isBadId2 \"123123\"" True  (isBadId2 "123123")
    test "isBadId2 \"111\"   " True  (isBadId2 "111")
    test "isBadId2 \"1234\"  " False (isBadId2 "1234")
    test "filterBadIds2 (95,115)" ["99", "111"]  (filterBadIds2 (95, 115))

    putStrLn "\n=== Example Input ==="
    example <- readFile "../inputs/Day02.example"
    case parseInput example of
        Left err -> print err
        Right rs -> do
            test "solve1 (example)" 1227775554 (solve1 rs)
            test "solve2 (example)" 4174379265 (solve2 rs)

    putStrLn "\n=== Real Input ==="
    input <- readFile "../inputs/Day02.txt"
    case parseInput input of
        Left err -> print err
        Right rs -> do
            test "solve1 (real)" 28146997880 (solve1 rs)
            test "solve2 (real)" 40028128307 (solve2 rs)
