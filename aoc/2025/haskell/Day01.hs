{-# OPTIONS_GHC -Wall #-}

import Data.List
    ( mapAccumL
    )

-- stack ghci --package pretty-simple ./Day01.hs

type Ring a = ([a], a, [a])


dial :: Ring Int
dial = ([], 0, [1..99])


start :: Ring Int
start = rotateR 50 dial


rotateR :: Int -> Ring a -> Ring a
rotateR n (ls, x, rs)
    | n == 0    = (ls, x, rs)
    | otherwise =
        rotateR
            (n-1)
            ( case rs of
                  []   ->
                      let
                          (y:ys) = reverse $ x:ls
                      in
                      ([], y, ys)
                  y:ys ->
                      (x:ls, y, ys)
            )


rotateL :: Int -> Ring a -> Ring a
rotateL n (ls, x, rs)
  | n == 0    = (ls, x, rs)
  | otherwise =
      rotateL
          (n-1)
          ( case ls of
                [] ->
                    let
                        (y:ys) = reverse $ x:rs
                    in
                    (ys, y, [])
                y:ys ->
                    (ys, y, x:rs)
          )


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

rotate :: String -> Ring Int -> Ring Int
rotate ('L' : xs) = rotateL (read xs)
rotate ('R' : xs) = rotateR (read xs)
rotate _ = error "rotate: bad input"

{-
*Main> readFile "Day01.input.txt" >>= print . solve1 . lines
1064
-}
-- readFile "Day01.input.txt" >>= print . solve1 . lines
solve1 :: [String] -> Int
solve1 = length . filter (\(_,n,_) -> n == 0) . scanl (flip rotate) start


simple :: Ring Int
simple = ([], 0, [1..9])


track :: (Int -> Ring Int -> Ring Int) -> Int -> Ring Int -> (Ring Int, Int)
track f n d =
    let
        res = take (n+1) $ iterate (f 1) d
        d' = res !! n
        m = length $ filter (== 0) $ fmap (\(_,x,_) -> x) (tail res)
    in
    (d', m)


rotateR' :: Int -> Ring Int -> (Ring Int, Int)
rotateR' =
    track rotateR


rotateL' :: Int -> Ring Int -> (Ring Int, Int)
rotateL' =
    track rotateL


-- rotate' "L3" simple
rotate' :: String -> Ring Int -> (Ring Int, Int)
rotate' ('L' : xs ) = rotateL' (read xs)
rotate' ('R' : xs ) = rotateR' (read xs)
rotate' _ = error "rotate': bad input"

{-
*Main> readFile "../inputs/Day01.txt" >>= print . solve2 . lines
6122
-}

solve2 :: [String] -> Int
solve2 =
    sum . snd . mapAccumL (flip rotate') start


{- Lesson learned.

In summary, `scanl` is like `foldl`, but we return all the states rather than just the last one.

*Main> scanl (\acc x -> acc + x) 0 [1,2,3,4,5]
[0,1,3,6,10,15]


And `mapAccumL` is like `foldl`, but you also emit an output at each step (collected in `snd`).

*Main> Data.List.mapAccumL (\acc x -> (acc + x, acc)) 0 [1,2,3,4,5]
(15,[0,1,3,6,10])

*Main> Data.List.mapAccumL (\acc x -> (acc + x, x > acc)) 0 [1,2,3,4,5]
(15,[True,True,False,False,False])


-}
