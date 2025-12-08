{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Function ((&))

example :: [String]
example =
    [ "L68"
    , "L30"
    , "R48"
    , "L5"
    , "R60"
    , "L55"
    , "L1"
    , "L99"
    , "R14"
    , "L82"
    ]

parse :: String -> Integer
parse ('L' : xs) = read xs * (-1)
parse ('R' : xs) = read xs
parse _ = error "parse: bad input"

_example' :: [Integer]
_example' = map parse example

{-

λ> readFile "../inputs/Day01.txt" <&> solve1 . lines
1064

-}

step1 :: Integer -> Integer -> Integer
step1 =
    \acc n -> snd $ (acc + n) `divMod` 100

solve1 :: [String] -> Int
solve1 lines' =
    length $
        filter ((==) 0) $
            scanl
                step1
                50
                (map parse lines')

step2 :: (Integer, (Integer, b)) -> Integer -> (Integer, (Integer, Integer))
step2 =
    \(oldScore, (oldPos, _)) n ->
        let
            clockwise = n > 0
            (crossings, newPos) = (oldPos + n) `divMod` 100
            score
                | oldPos  < 0 && newPos == 0                  = abs crossings + 1
                | oldPos  > 0 && newPos == 0 && not clockwise = abs crossings + 1
                | oldPos == 0                && not clockwise = abs crossings - 1
                | otherwise = abs crossings

         in
            (oldScore + score, (newPos, crossings))

{-

λ> (0, (50,0)) & step2' (-68) & step2' (-30) & step2' 48 & step2' (-5) & step2' 60 & step2' (-55) & step2' (-1) & step2' (-99) & step2' 14 & step2' (-82)
(6,(32,-1))

-}
step2' :: Integer -> (Integer, (Integer, b)) -> (Integer, (Integer, Integer))
step2' = flip step2


solve2 :: [String] -> Integer
solve2 lines' =
    fst $
        foldl
            step2
            (0, (50, 0))
            (map parse lines')

{-

λ> (0,(50,0)) & step2' (-82) & step2' (-30)
(1,(38,0))

λ> readFile "../inputs/Day01.example" <&> solve2' . lines
[(0,(50,0)),(1,(82,-1)),(1,(52,0)),(2,(0,1)),(2,(95,-1)),(3,(55,1)),(4,(0,0)),(4,(99,-1)),(5,(0,0)),(
5,(14,0)),(6,(32,-1))]

-}
_solve2' :: [String] -> [(Integer, (Integer, Integer))]
_solve2' lines' =
    scanl
        step2
        (0, (50, 0))
        (map parse lines')


main :: IO ()
main = do
    let test desc expected actual =
            putStrLn
                ( if expected == actual
                    then
                        "\ESC[32m" ++ "✓ " ++ desc
                    else
                        "\ESC[31m" ++ "✗ " ++ desc ++ " => " ++ show expected ++ " /= " ++ show actual
                ) >> putStr "\ESC[0m"

    txt <- lines <$> readFile "../inputs/Day01.txt"
    exa <- lines <$> readFile "../inputs/Day01.example"

    test "+89 -> R11 is 1 crossing"
        (1, (0,1))
        ((0, (89,0)) & step2' 11)

    test "+41 -> L41 is 1 crossing"
        (1, (0, 0))
        ((0, (41, 0)) & step2' (-41))

    test "-41 -> R41 is 1 crossing"
        (1, (0, 0))
        ((0, ((-41), 0)) & step2' 41)

    test "+41 -> L41 is 1 crossing"
        (1, (0, 0))
        ((0, ((-41), 0)) & step2' 41)

    test "+89 -> R111"
        (2, (0,2))
        ((0, (89,0)) & step2' 111)

    test "-41 -> R141 is 2 crossings"
        (2, (0, 1))
        ((0, ((-41), 0)) & step2' 141)

    test "back to 0 from right, twice"
        (2, (0, (-1)))
        ((0, (41, 0)) & step2' (-141))

    test "0 -> (-698"
        (6, (2, -7))
        ((0, (0, 0)) & step2' (-698))

    test "0 -> L5 is 0 crossings"
        (0, (95,(-1)))
        ((0, (0,0)) & step2' (-5))

    test "example (part 1)" 3 (solve1 exa)
    test "real (part 1)" 1064 (solve1 txt)

    test "example (part 2)" 6 (solve2 exa)
    test "real (part 2)" 6122 (solve2 txt)

    -- arrive on 89, then R911
    test "edge case 1" 16 (solve2 $ exa ++ ["R57", "R911"])
