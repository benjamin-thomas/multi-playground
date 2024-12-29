{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main (main) where

import Data.Array (
  Array,
  Ix (inRange),
  bounds,
  listArray,
  (!),
  (//),
 )

import Control.Monad (when)
import Data.List (nub, unfoldr)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

type Vec2 = (Int, Int)
type Grid = Array Vec2 Char

data Guard = Guard
  { pos :: Vec2
  , dir :: Vec2
  }
  deriving (Show, Eq)

-- Helper function to show direction as an arrow
dirToChar :: Vec2 -> Char
dirToChar (-1, 0) = '^' -- up
dirToChar (1, 0) = 'v' -- down
dirToChar (0, -1) = '<' -- left
dirToChar (0, 1) = '>' -- right
dirToChar _ = '?'

-- Enhanced grid visualization that shows the guard's position and direction
showGridWithGuard :: Grid -> Guard -> Int -> String
showGridWithGuard grid guard step =
  unlines $
    ["Step " ++ show step, "Position: " ++ show guard.pos ++ " Direction: " ++ show guard.dir]
      ++ [unlines rows]
 where
  (yMax, xMax) = snd $ bounds grid
  rows = [[showElem y x | x <- [0 .. xMax]] | y <- [0 .. yMax]]
  showElem y x
    | (y, x) == guard.pos = dirToChar guard.dir -- Show guard's direction
    | otherwise = grid ! (y, x)

-- Interactive walking function that waits for user input between steps
walkInteractive :: Grid -> Guard -> IO [Vec2]
walkInteractive grid = go 0 -- Start with step 0
 where
  go stepNum guard = do
    -- Show the current state before asking for input
    putStr "\ESC[2J" -- Clear screen
    putStr "\ESC[H" -- Move cursor to top-left
    putStrLn $ showGridWithGuard grid guard stepNum

    putStrLn "\nPress ENTER to make a step (or type 'q' to quit)"
    hFlush stdout
    input <- getLine

    case input of
      "q" -> return []
      _ -> case step grid guard of
        Nothing -> do
          putStrLn "Walk complete!"
          return [guard.pos]
        Just (pos, guard') -> do
          rest <- go (stepNum + 1) guard' -- Increment step counter
          return (pos : rest)

main :: IO ()
main = do
  putStr "\ESC[2J" -- Clear screen
  putStr "\ESC[H" -- Move cursor to top-left

  -- Check for INTERACTIVE environment variable
  isInteractive <- fmap (fromMaybe "0") (lookupEnv "INTERACTIVE")
  let interactive = isInteractive == "1"

  -- Print mode information
  putStrLn $ "Running in " ++ (if interactive then "interactive" else "normal") ++ " mode..."

  str <- readFile "../../_inputs/06.example"
  let grid = mkGrid str
  let start = findStartExn str
  let guard = Guard start (-1, 0)

  -- Use the appropriate walking function based on mode
  positions <- walkWithMode interactive grid guard

  putStrLn $ "Part 1 result: " ++ show (length $ nub positions)
  putStrLn $ "Part 2 result: " ++ show (part2 grid guard)

-- Unified walking interface that handles both interactive and non-interactive modes
walkWithMode :: Bool -> Grid -> Guard -> IO [Vec2]
walkWithMode True = walkInteractive -- Interactive mode
walkWithMode False = walkNormal -- Normal mode

-- Non-interactive walk wrapped in IO for consistency
walkNormal :: Grid -> Guard -> IO [Vec2]
walkNormal grid = return . walk grid

-- Regular non-interactive walk for the base case and part2
walk :: Grid -> Guard -> [Vec2]
walk grid = unfoldr step'
 where
  step' g = case step grid g of
    Nothing -> Nothing
    Just (pos, g') -> Just (pos, g')

part2 :: Grid -> Guard -> Int
part2 grid guard = length $ filter (isLoop guard []) modifiedGrids
 where
  modifiedGrids =
    [ grid // [(new, 'O')]
    | new <- news
    , new /= guard.pos
    ]
  news = nub $ walk grid guard

turnRight :: Vec2 -> Vec2
turnRight (x, y) = (y, -x)

(.+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(.+.) (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

step :: Grid -> Guard -> Maybe (Vec2, Guard)
step grid guard
  | not (inRange (bounds grid) guard.pos) = Nothing
  | not (inRange (bounds grid) ahead) = Just (guard.pos, guard{pos = ahead})
  | (grid ! ahead) `elem` ['#', 'O'] = Just (guard.pos, guard{dir = turnRight guard.dir})
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
  listArray
    ((0, 0), (y, x))
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