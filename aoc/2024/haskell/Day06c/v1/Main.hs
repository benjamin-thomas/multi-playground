{-# LANGUAGE OverloadedRecordDot #-}

-- From https://work.njae.me.uk/2024/12/06/advent-of-code-2024-day-6/
-- https://codeberg.org/NeilNjae/advent-of-code-24/src/branch/main/advent06

module Main (main) where

import Data.Array (
  Array,
  Ix (inRange),
  bounds,
  listArray,
  (!),
  (//),
 )

import Data.List (nub, unfoldr)
import Data.Maybe (fromJust, isNothing)

type Vec2 = (Int, Int)
type Grid = Array Vec2 Bool

data Guard = Guard
  { pos :: Vec2
  , dir :: Vec2
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Running v1..."
  str <- readFile "../../_inputs/06.txt"
  let grid = mkGrid str
  let start = findStartExn str
  let guard = Guard start (-1, 0)
  print $ part1 grid guard
  print $ part2 grid guard

part1, part2 :: Grid -> Guard -> Int
part1 grid guard = length $ nub $ walk grid guard
part2 grid guard = length $ filter (isLoop guard []) modifiedGrids
 where
  modifiedGrids =
    [ grid // [(new, True)]
    | new <- news
    , new /= guard.pos
    ]
  news = nub $ walk grid guard

turnRight :: Vec2 -> Vec2
turnRight (x, y) = (y, -x)

walk :: Grid -> Guard -> [Vec2]
walk grid = unfoldr (step grid)

(.+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(.+.) (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

step :: Grid -> Guard -> Maybe (Vec2, Guard)
step grid guard
  | not (inRange (bounds grid) guard.pos) = Nothing
  | not (inRange (bounds grid) ahead) = Just (guard.pos, guard{pos = ahead})
  | grid ! ahead = Just (guard.pos, guard{dir = turnRight guard.dir})
  | otherwise = Just (guard.pos, guard{pos = ahead})
 where
  ahead = guard.pos .+. guard.dir

isLoop :: Guard -> [Guard] -> Grid -> Bool
isLoop guard trail grid
  | isNothing stepped = False
  | hasTurned && guard `elem` trail = True
  | hasTurned = isLoop guard' (guard : trail) grid
  | otherwise = isLoop guard' trail grid
 where
  stepped = step grid guard
  (_, guard') = fromJust stepped
  hasTurned = guard.dir /= guard'.dir

mkGrid :: String -> Grid
mkGrid text =
  listArray ((0, 0), (y, x)) $
    fmap
      (== '#')
      (concat rows)
 where
  rows = lines text
  y = length rows - 1
  x = length (head rows) - 1

findStartExn :: String -> Vec2
findStartExn text =
  head $
    [ (y, x)
    | x <- [0 .. maxC]
    , y <- [0 .. maxR]
    , rows !! y !! x == '^'
    ]
 where
  rows = lines text
  maxR = length rows - 1
  maxC = length (head rows) - 1

{-
showGrid :: Grid -> String
showGrid grid = unlines rows
 where
  (yMax, xMax) = snd $ bounds grid :: (Int, Int)

  rows :: [[Char]]
  rows = [showRow y | y <- [0 .. yMax]]

  showRow :: Int -> [Char]
  showRow y = [showElem y x | x <- [0 .. xMax]]

  showElem :: Int -> Int -> Char
  showElem y x =
    if grid ! (y, x)
      then '#'
      else '.'
 -}