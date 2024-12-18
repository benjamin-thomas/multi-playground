{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- cabal:

build-depends: base
-}

{-

Terminal 1
==========
./repl
> :cmd return $ unlines [":!clear",":reload", "answer1 example"]

Terminal 2
==========
find *.hs | entr tmux send-keys -t aoc:0 Up Enter

 -}

module Day04 (main, example) where

import Data.Bifunctor
import Data.List (isPrefixOf, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe

main :: IO ()
main = do
    input <- readFile "../_inputs/04.txt"
    print $ answer1 input
    print $ answer2 input

data ToCheck a = ToCheck
    { north :: a
    , northEast :: a
    , east :: a
    , southEast :: a
    , south :: a
    , southWest :: a
    , west :: a
    , northWest :: a
    }
    deriving (Show, Functor)

-- mapToCheck :: ([(Int, Int)] -> [(Int, Int)]) -> ToCheck -> ToCheck
-- mapToCheck
--     f
--     ( ToCheck
--             { north = north'
--             , northEast = northEast'
--             , east = east'
--             , southEast = southEast'
--             , south = south'
--             , southWest = southWest'
--             , west = west'
--             , northWest = northWest'
--             }
--         ) =
--         ToCheck
--             { north = f north'
--             , northEast = f northEast'
--             , east = f east'
--             , southEast = f southEast'
--             , south = f south'
--             , southWest = f southWest'
--             , west = f west'
--             , northWest = f northWest'
--             }

toCheck2 :: (Int, Int) -> ToCheck [(Int, Int)]
toCheck2 (x, y) =
    ToCheck
        { north = bimap (+ x) (+ y) <$> [(0, 0), (0, -1), (0, -2), (0, -3)]
        , northEast = bimap (+ x) (+ y) <$> [(0, 0), (1, -1), (2, -2), (3, -3)]
        , east = bimap (+ x) (+ y) <$> [(0, 0), (1, 0), (2, 0), (3, 0)]
        , southEast = bimap (+ x) (+ y) <$> [(0, 0), (1, 1), (2, 2), (3, 3)]
        , south = bimap (+ x) (+ y) <$> [(0, 0), (0, 1), (0, 2), (0, 3)]
        , southWest = bimap (+ x) (+ y) <$> [(0, 0), (-1, 1), (-2, 2), (-3, 3)]
        , west = bimap (+ x) (+ y) <$> [(0, 0), (-1, 0), (-2, 0), (-3, 0)]
        , northWest = bimap (+ x) (+ y) <$> [(0, 0), (-1, -1), (-2, -2), (-3, -3)]
        }

checkToList :: ToCheck a -> [a]
checkToList
    ToCheck
        { north
        , northEast
        , east
        , southEast
        , south
        , southWest
        , west
        , northWest
        } =
        [ north
        , northEast
        , east
        , southEast
        , south
        , southWest
        , west
        , northWest
        ]

{-

X M A S -> For every point, there are 8 potential directions to check:

up, up-right, right, down-right, down, down-left, left, up-left

So I have 10x10 points, so I need to check 10x10x8 positions

 -}

{-

>>> (\(a,b) -> (0+a, 0+b)) <$> [(0, 0), (0, 1), (0, 2), (0, 3)]
[(0,0),(0,1),(0,2),(0,3)]

>>> (\(n, c) -> ((n,1),c) ) <$> zip [0..] (example !! 1)
[((0,1),'M'),((1,1),'S'),((2,1),'A'),((3,1),'M'),((4,1),'X'),((5,1),'M'),((6,1),'S'),((7,1),'M'),((8,1),'S'),((9,1),'A')]

 -}
{- FOURMOLU_DISABLE -}
toCheck :: [(Int, Int)]
toCheck =
    [ (0, 0), (-1,  0), (-2,  0), (-3,  0) -- up
    , (0, 0), (-1,  1), (-2,  2), (-3,  3) -- up-right
    , (0, 0), ( 0,  1), ( 0,  2), ( 0,  3) -- right
    , (0, 0), ( 1,  1), ( 2,  2), ( 3,  3) -- down-right
    , (0, 0), ( 1,  0), ( 2,  0), ( 3,  0) -- down
    , (0, 0), ( 1, -1), ( 2, -2), ( 3, -3) -- down-left
    , (0, 0), (-1,  0), (-2,  0), (-3,  0) -- left
    , (0, 0), (-1, -1), (-2, -2), (-3, -3) -- up-left
    ]





directions :: [(Int, Int)]
directions =
    [ ( 0, -1) -- up
    , ( 1, -1) -- up-right
    , ( 1,  0) -- right
    , ( 1,  1) -- down-right
    , ( 0,  1) -- down
    , (-1,  1) -- down-left
    , (-1,  0) -- left
    , (-1, -1) -- up-left
    ]
{- FOURMOLU_ENABLE -}

-- generateOffsets :: Int -> [(Int, Int)]
-- generateOffsets n = concatMap (\(dx, dy) -> [(k * dx, k * dy) | k <- [0 .. n - 1]]) directions
--   where
--     directions = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

example :: [[Char]]
example =
    [ ['M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M']
    , ['M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A']
    , ['A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M']
    , ['M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X']
    , ['X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M'] -- 4
    , ['X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A']
    , ['S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S']
    , ['S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A']
    , ['M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M']
    , ['M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X']
    ]

{-
FIXME: for each position in the 2D table, return the actual score!

 -}
example2 :: [[Char]]
example2 =
    [ ['1', '2', '3', '4']
    , ['5', '6', '7', '8']
    , ['9', 'A', 'B', 'C']
    , ['D', 'E', 'F', 'G']
    ]

wip :: [String] -> (Int, Int) -> ToCheck [Char]
wip strs (x, y) = fmap (fromMaybe '.') <$> fmap (`Map.lookup` (dict strs)) <$> toCheck2 (x, y)

answer1WIP :: [String] -> Int
answer1WIP strs =
    sum
        $ (\strs -> countXmas4 <$> strs)
            <$> concat
        $ checkToList <$> wip strs <$> fst <$> kv strs

{-

>>> Map.lookup (0, 0) dict
Just 'M'

>>> Map.lookup (1, 1) dict
Just 'S'

 -}
dict :: [String] -> Map (Int, Int) Char
dict str = Map.fromList (kv str)

kv :: [String] -> [((Int, Int), Char)]
kv str =
    [ ((x, y), c)
    | (y, (row :: String)) <- zip [0 ..] str
    , (x, (c :: Char)) <- zip [0 ..] row
    ]

kv' :: [[((Int, Int), Char)]]
kv' =
    fmap
        ( \(y, row) ->
            fmap
                (\(x, c) -> ((x, y), c))
                (zip [0 ..] row)
        )
        (zip [0 ..] example)

genToCheck :: (Int, Int) -> [(Int, Int)]
genToCheck (x, y) = [(x + x', y + y') | (x', y') <- toCheck]

-- combinations :: (Int, Int) -> [[(Int, Int)]]
-- combinations :: [[(Int, Int)]]
-- positionsToCheck :: [[Maybe Char]]
-- positionsToCheck = fmap search . genToCheck <$> Map.keys dict

-- combinations :: [String]
-- combinations = catMaybes <$> positionsToCheck

advance :: (Int, String) -> (Int, String)
advance (count, 'X' : 'M' : 'A' : 'S' : xs) = (count + 1, xs)
advance (count, _ : xs) = (count, xs)
advance (count, []) = (count, [])

countXmas :: String -> Int
countXmas str =
    go (0, str)
  where
    go (n, []) = n
    go (n, xs) = go $ advance (n, xs)

countXmas2 :: String -> Int
countXmas2 str = length $ filter (isPrefixOf "XMAS") (tails str)

countXmas3 :: String -> Int
countXmas3 [] = 0
countXmas3 str@(_ : xs)
    | "XMAS" `isPrefixOf` str = 1 + countXmas3 xs
    | otherwise = countXmas3 xs

countXmas4 :: String -> Int
countXmas4 = \case
    [] -> 0
    ('X' : 'M' : 'A' : 'S' : xs) -> 1 + countXmas4 xs
    _ : xs -> countXmas4 xs

countXmas5 :: String -> Int
countXmas5 = snd . foldr step ([], 0)
  where
    step x (buffer, count) =
        let newBuffer = take 4 (x : buffer) -- Maintain the last 4 characters
         in if newBuffer == "XMAS"
                then (newBuffer, count + 1)
                else (newBuffer, count)

-- countXmas6 :: String -> Int
-- countXmas6 = snd . foldl step ("", 0)
--   where
--     step (window, count) char =
--         let newWindow = drop 1 (window ++ [char]) -- Maintain a sliding window of the last 4 characters
--          in if newWindow == "XMAS"
--                 then (newWindow, count + 1)
--                 else (newWindow, count)

-- search :: (Int, Int) -> Maybe Char
-- search = flip Map.lookup dict

answer1 :: String -> Int
answer1 = answer1WIP . lines

answer2 :: String -> Int
answer2 =
    const 0

-- score (x, y) = fmap (flip Map.lookup dict) (genToCheck (x, y))

-- wat = Map.mapWithKey (\k v -> (v, genToCheck k)) dict