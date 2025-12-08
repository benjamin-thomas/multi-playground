{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (mapAccumL)

import Text.ParserCombinators.ReadP
    ( ReadP
    , char
    , choice
    , eof
    , optional
    , readP_to_S
    , readS_to_P
    , sepBy
    )

data Dir
    = L Int
    | R Int
    deriving (Show)

dirToInt :: Dir -> Int
dirToInt (L n) = -n
dirToInt (R n) = n

-- Parsing

int :: ReadP Int
int = readS_to_P reads

dir :: ReadP Dir
dir = choice [L <$ char 'L', R <$ char 'R'] <*> int

dirs :: ReadP [Dir]
dirs = dir `sepBy` char '\n' <* optional (char '\n') <* eof

parseOrDie :: ReadP a -> String -> a
parseOrDie p s = case readP_to_S p s of
    [(x, "")] -> x
    _ -> error "parse failed"

-- Part 1: Count how many times we land on position 0

step1 :: Int -> Int -> Int
step1 pos delta = snd $ (pos + delta) `divMod` 100


{-

λ> readFile "../inputs/Day01.txt" <&> solve1 . parseOrDie dirs
1064

-}
solve1 :: [Dir] -> Int
solve1 =
    length
        . filter (== 0)
        . scanl step1 50
        . map dirToInt

-- Part 2: Count crossings over position 0

step2 :: (Int, Int) -> Int -> (Int, Int)
step2 (oldScore, oldPos) n =
    let
        clockwise = n > 0
        (crossings, newPos) = (oldPos + n) `divMod` 100
        score
            | oldPos < 0 && newPos == 0 = abs crossings + 1
            | oldPos > 0 && newPos == 0 && not clockwise = abs crossings + 1
            | oldPos == 0 && not clockwise = abs crossings - 1
            | otherwise = abs crossings
     in
        (oldScore + score, newPos)

{-

λ> readFile "../inputs/Day01.txt" <&> solve2 . parseOrDie dirs
6122

-}
solve2 :: [Dir] -> Int
solve2 = fst . foldl step2 (0, 50) . map dirToInt


{-

Conclusion...

Using a parser was nice, but didn't bring me any closer to being able to solve the problem via module arithmetic (found the solution via Day01Alt2.hs)

-}
