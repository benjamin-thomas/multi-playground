{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Day06 where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map

yellow :: String
yellow = "\ESC[1;33m"

gray :: String
gray = "\ESC[1;90m"

reset :: String
reset = "\ESC[0m"

makeGrid :: String -> Map (Int, Int) Char
makeGrid input =
    Map.fromList $
        [ ((y, x), c)
        | (y, line) <- zip [0 ..] (lines input)
        , (x, c) <- zip [0 ..] line
        ]

findGuardPos :: Map (Int, Int) Char -> Maybe (Int, Int)
findGuardPos =
    Map.foldlWithKey'
        (\acc coord v -> if v == '^' then Just coord else acc)
        Nothing

lookupExn :: Map (Int, Int) Char -> (Int, Int) -> Char
lookupExn grid (y, x) = maybe (error "Out of bounds") id $ Map.lookup (y, x) grid

printGridExn :: State -> IO ()
printGridExn state = do
    let (height, width) = stGridDims state
    let grid = stGrid state
    let guardPos = stGuardPos state
    forM_ [0 .. height] $ \y -> do
        forM_ [0 .. width] $ \x -> do
            when ((y, x) == guardPos) $ putStr yellow
            let char = lookupExn grid (y, x)
            when (char == 'X') $ putStr gray
            putChar char
            when (char == 'X') $ putStr reset
            when ((y, x) == guardPos) $ putStr reset
        putStrLn ""

data State = State
    { stIteration :: Int
    , stGuardPos :: (Int, Int)
    , stGuardDir :: (Int, Int)
    , stGrid :: Map (Int, Int) Char
    , stGridDims :: (Int, Int)
    , stIsDone :: Bool
    }

(.+.) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(.+.) (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

guardIcon :: (Int, Int) -> Char
guardIcon = \case
    (-1, 0) -> '^'
    (0, 1) -> '>'
    (1, 0) -> 'v'
    (0, -1) -> '<'
    _ -> error "Invalid direction"

rotateGuard :: (Int, Int) -> (Int, Int)
rotateGuard = \case
    (-1, 0) -> (0, 1)
    (0, 1) -> (1, 0)
    (1, 0) -> (0, -1)
    (0, -1) -> (-1, 0)
    _ -> error "Invalid direction"

update :: State -> State
update state =
    do
        let oldGrid = stGrid state
        let oldGuardPos = stGuardPos state
        let oldGuardDir = stGuardDir state

        let (newGuardPos, newGuardDir, isDone) =
                let candidatePos = oldGuardPos .+. oldGuardDir
                 in case Map.lookup candidatePos oldGrid of
                        Nothing -> (oldGuardPos, oldGuardDir, True)
                        Just '#' -> (oldGuardPos, rotateGuard oldGuardDir, False)
                        _ -> (candidatePos, oldGuardDir, False)

        let newGrid :: Map (Int, Int) Char
            newGrid =
                oldGrid
                    & Map.update (const $ Just $ guardIcon newGuardDir) newGuardPos
                    & bool
                        (Map.update (const $ Just 'X') oldGuardPos)
                        id
                        (newGuardPos == oldGuardPos)

        state
            { stIteration = stIteration state + 1
            , stGuardPos = newGuardPos
            , stGuardDir = newGuardDir
            , stGrid = newGrid
            , stIsDone = isDone
            }

countTrail :: Map k Char -> Int
countTrail = Map.foldl' (\tot v -> tot + (if v == 'X' then 1 else 0)) 0

newtype VisualMode = VisualMode Bool

gameLoop :: VisualMode -> State -> IO ()
gameLoop visualMode state = do
    putStr "\ESC[2J\ESC[H"
    when (coerce visualMode) $ do
        putStrLn $ "Iteration: " ++ show (stIteration state)
        putStrLn ""
        putStr "Guard is at pos: " >> print (stGuardPos state)
        putStr "Trail count: " >> print (countTrail $ stGrid state) >> putStrLn ""
        printGridExn state
        threadDelay 70_000
    let newState = update state
    if not (stIsDone newState)
        then
            gameLoop visualMode newState
        else
            putStr "\nExiting with trail count of: " >> print (countTrail (stGrid state) + 1)

main :: IO ()
main = do
    let visualMode = VisualMode False
    -- example <- readFile "../_inputs/06.example" -- 41
    example <- readFile "../_inputs/06.txt" -- 5404
    let grid = makeGrid example
    let guardPos = maybe (error "Guard not found") id (findGuardPos grid)
    let (height, width) =
            let keys = Map.keys grid
             in ( maximum $ fst <$> keys
                , maximum $ snd <$> keys
                )

    let initState =
            State
                { stIteration = 0
                , stGuardPos = guardPos
                , stGuardDir = (-1, 0)
                , stGrid = grid
                , stGridDims = (height, width)
                , stIsDone = False
                }

    gameLoop visualMode initState
