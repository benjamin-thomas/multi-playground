{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.Parsec
import Data.List (sort)
import Data.Bool (bool)


input :: Parser ([(Int,Int)], [Int])
input =
    (,)
    <$> ranges
    <*> (char '\n' *> ids)


ids :: Parser [Int]
ids = int `sepEndBy` char '\n'


int :: Parser Int
int = read <$> many1 digit


range :: Parser (Int, Int)
range =
    (,)
    <$> int
    <*> (char '-' *> int)


ranges :: Parser [(Int, Int)]
ranges = range `sepEndBy` char '\n'


part1 :: ([(Int,Int)], [Int]) -> Int
part1 (rs, is) =
    length $ filter isFresh is
  where
    inRange n (lo,hi) = lo <= n && n <= hi
    isFresh n = any (inRange n) rs


fuse :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
fuse a@(lo,hi) b@(lo',hi')
    | a > b     = fuse b a               -- fusion requires a ≤ b
    | hi >= lo' = Just (lo, max hi hi')  -- do fuse
    | otherwise = Nothing                -- cannot fuse


fuseAll :: Ord a => [(a,a)] -> [(a,a)]
fuseAll [] = []
fuseAll [x] = [x]
fuseAll (x:y:rest) =
    case fuse x y of
        Just fused ->
            fuseAll (fused:rest)
        Nothing ->
            x : fuseAll (y:rest)


part2 :: (Num a, Ord a) => [(a, a)] -> a
part2 rs =
    sum . map cnt . fuseAll . sort $ rs
  where
    cnt (lo,hi) = hi-lo + 1


main :: IO ()
main = do
    let confirm = bool "BAD" "OK"

    --print =<< readFile "../inputs/Day05.example"
    putStrLn "---"
    putStr "Example (part 1) : "
    putStrLn =<< confirm . (== Right  3) . fmap         part1 . flip parse "" input <$> readFile "../inputs/Day05.example"
    putStr "Example (part 2) : "
    putStrLn =<< confirm . (== Right 14) . fmap (part2 . fst) . flip parse "" input <$> readFile "../inputs/Day05.example"

    putStrLn "---"
    putStr "Real (part 1)    : "
    putStrLn =<< confirm . (== Right 638) .             fmap        part1 . flip parse "" input <$> readFile "../inputs/Day05.txt"

    putStr "Real (part 2)    : "
    putStrLn =<< confirm . (== Right 352946349407338) . fmap (part2 . fst) . flip parse "" input <$> readFile "../inputs/Day05.txt"
    putStrLn "---"
