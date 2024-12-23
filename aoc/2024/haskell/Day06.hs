{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Day06 where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Bifunctor (Bifunctor (first))
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
        putStrLn ""
        forM_ [0 .. width] $ \x -> do
            when ((y, x) == guardPos) $ putStr yellow
            let char = lookupExn grid (y, x)
            when (char == 'X') $ putStr gray
            putChar char
            when (char == 'X') $ putStr reset
            when ((y, x) == guardPos) $ putStr reset

data State = State
    { stIteration :: Int
    , stGuardPos :: (Int, Int)
    , stGrid :: Map (Int, Int) Char
    , stGridDims :: (Int, Int)
    }

update :: State -> State
update state = do
    let grid = stGrid state
    let oldGuardPos = stGuardPos state
    let newGuardPos = case Map.lookup (first pred oldGuardPos) grid of
            Nothing -> oldGuardPos
            Just '#' -> oldGuardPos
            _ -> first pred oldGuardPos

    let newGrid1 = Map.update (\_ -> Just '^') newGuardPos grid
    let newGrid2 =
            if newGuardPos == oldGuardPos
                then newGrid1
                else
                    Map.update (\_ -> Just 'X') oldGuardPos newGrid1
    state
        { stIteration = stIteration state + 1
        , stGuardPos = newGuardPos
        , stGrid = newGrid2
        }

gameLoop :: State -> IO ()
gameLoop state = do
    putStr "\ESC[2J\ESC[H"
    putStrLn $ "Iteration: " ++ show (stIteration state)
    putStrLn "---"
    putStr "Guard is at pos: " >> print (stGuardPos state)
    printGridExn state
    threadDelay 1000_000
    let newState = update state
    gameLoop newState

main :: IO ()
main = do
    example <- readFile "../_inputs/06.example"
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
                , stGrid = grid
                , stGridDims = (height, width)
                }

    gameLoop initState
