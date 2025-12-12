{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-

λ> readFile "../inputs/Day03.example" <&> flip parse "" rows >>= traverse_ (traverse_ print)
[9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]
[8,1,1,1,1,1,1,1,1,1,1,1,1,1,9]
[2,3,4,2,3,4,2,3,4,2,3,4,2,7,8]
[8,1,8,1,8,1,9,1,1,1,1,2,1,1,1]

-}

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)

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
            putStrLn $ "  " ++ red   ++ "✗"  ++ reset
            putStrLn $ "  " ++ red   ++ "left : " ++ show l ++ reset
            putStrLn $ "  " ++ red   ++ "right: " ++ show r ++ reset


digit' :: Parser Int
digit' = digitToInt <$> digit


rows :: Parser [[Int]]
rows = (many1 digit' `sepEndBy` char '\n')


pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs


expand2 :: Num a => (a,a) -> a
expand2 (a, b) = a*10 + b


largest :: (Ord a, Num a) => [a] -> a
largest row = maximum $ map expand2 (pairs row)


{-

λ> solve1 <$> readFile "../inputs/Day03.txt"
Right 17179

-}
solve1 :: String -> Either ParseError Int
solve1 input =
    fmap
        (sum . map largest)
        (parse rows "" input)


-- Creates combinations (n choose k)
choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs


expand12 :: Num a => [a] -> a
expand12 [a, b, c, d, e, f, g, h, i, j, k, l]
    = a*100000000000
    + b*10000000000
    + c*1000000000
    + d*100000000
    + e*10000000
    + f*1000000
    + g*100000
    + h*10000
    + i*1000
    + j*100
    + k*10
    + l*1
expand12 _ = error "expand12: bad value"


-- Attempt at solving part2 using combinations as I did in part 1... way too slow!
_largest2combi :: (Num a, Ord a) => [a] -> a
_largest2combi row = maximum $ map expand12 (choose 12 row)


solve2 :: String -> Either ParseError Int
solve2 input =
    fmap
        --(sum . map largest2combi)
        (sum . map largest2greedy)
        (parse rows "" input)


findMax :: (Ord a, Num a) => [a] -> (a,a)
findMax =
    (\(_, pos, val) -> (pos,val))
    . foldl
        (\(idx,pos, n) m ->
            if m > n then
                (idx+1, idx, m)
            else
                (idx+1, pos, n)
        )
        (0,0,0)


-- Enables solving part2 (using combinations was way too slow)
largest2greedy :: [Int] -> Int
largest2greedy ns = expand12 $ pickGreedy (length ns) 12 0 ns

-- For len=100 digits, and desired n=12 size of the digit...
--  1) we pick the best number in the range [0,88]
--  2) we pick the best number in the range [?,89]
--  3) we pick the best number in the range [?,90]
-- 12) we pick the best number in the range [?,99]
-- Where ? corresponds to the index of the prior match
--
-- Computing the first item, we apply findMax over this range:
-- [0..88] == let xs = [0..99] in take 89 $ drop 0 xs
pickGreedy :: Int -> Int -> Int -> [Int] -> [Int]
pickGreedy   _ 0 _   _ = []
pickGreedy len n m lst =
    let (m', n') = findMax . take (len-n-m+1) . drop m $ lst in
    n' : pickGreedy len (n-1) (m+m'+1) lst


-- MAIN

main :: IO ()
main = do
    putStrLn "=== Parsing ==="
    test "parse rows (example)"
         (Right [[9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]])
         (parse rows "" "987654321111111\n")

    putStrLn "\n=== Part 1 ==="

    test "pairs"
         [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
         (pairs [1,2,3,4])

    test "expand2"
         98
         (expand2 (9,8))

    test "largest [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1]"
         98
         (largest [9,8,7,6,5,4,3,2,1,1,1,1,1,1,1])


    putStrLn "\n=== Part 2 ==="

    test "findMax [8,1,8]" (0,8) (findMax [8,1,8])
    test "findMax [1,8,1]" (1,8) (findMax [1,8,1])

    test "pickGreedy 15 12 0 [8,1,8,1,8,1,9,1,1,1,1,2,1,1,1]"
         [8,8,8,9,1,1,1,1,2,1,1,1]
         (pickGreedy 15 12 0 [8,1,8,1,8,1,9,1,1,1,1,2,1,1,1])

    test "expand12 [8,8,8,9,1,1,1,1,2,1,1,1]"
         888911112111
         (expand12 [8,8,8,9,1,1,1,1,2,1,1,1])



    putStrLn "\n=== Example Input ==="
    example <- readFile "../inputs/Day03.example"
    test "solve1 (example)" (Right 357)           (solve1 example)
    test "solve2 (example)" (Right 3121910778619) (solve2 example)


    putStrLn "\n=== Real Input ==="
    real <- readFile "../inputs/Day03.txt"
    test "solve1 (real)" (Right 17179)           (solve1 real)
    test "solve2 (real)" (Right 170025781683941) (solve2 real)
