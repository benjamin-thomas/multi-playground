#!/usr/bin/env cabal

{-# HLINT ignore "Use fromMaybe" #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
NOTE: although I could "execute" the file due to the shebang, "cabal run" is
      still required due to the injection of the extra ghc options.
      Also, having that shebang seems to make the vscode extension happy.

Run the optimized version with:
  cabal run ./Day06_alt.hs --ghc-options "-main-is Day06_alt -O2"

Debug perf problems with:
  cabal run ./Day06_alt.hs --ghc-options "-main-is Day06_alt -O2 -prof -fprof-auto -rtsopts -with-rtsopts=-P"

Develop with:
  I couldn't find a way to make ghcid work with that setup, so:

  Terminal 1:
    cabal repl ./Day06_alt.hs

  Terminal 2:
    find *.hs | entr tmux send-keys -t aoc:0 ':cmd return $ unlines [":!clear",":reload"]' Enter
 -}

{- cabal:
build-depends: base, containers, transformers, vector
-}

module Day06_alt where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, when)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as V
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stdin)

yellow :: String
yellow = "\ESC[1;33m"

gray :: String
gray = "\ESC[1;90m"

reset :: String
reset = "\ESC[0m"

-- Helper function to move cursor to start and clear downward
clearFromCursor :: IO ()
clearFromCursor = putStr "\ESC[H\ESC[J"

-- Helper to hide cursor during updates
hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

-- Helper to show cursor when done
showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

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

newtype Slots
    = Slots
        ( Maybe (Int, Int)
        , Maybe (Int, Int)
        , Maybe (Int, Int)
        , Maybe (Int, Int)
        , Maybe (Int, Int)
        )
    deriving (Show)

data State = State
    { stIteration :: Int
    , stGuardPos :: (Int, Int)
    , stGuardDir :: (Int, Int)
    , stGuardSlots :: Slots -- circle detection
    , stGrid :: Map (Int, Int) Char
    , stGridDims :: (Int, Int)
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

firstSlot :: Slots -> Maybe (Int, Int)
firstSlot (Slots (a, _, _, _, _)) = a

insertSlot :: Slots -> (Int, Int) -> Slots
insertSlot (Slots (Nothing, Nothing, Nothing, Nothing, Nothing)) pos = Slots (Just pos, Nothing, Nothing, Nothing, Nothing)
insertSlot (Slots (Just a, Nothing, Nothing, Nothing, Nothing)) pos = Slots (Just a, Just pos, Nothing, Nothing, Nothing)
insertSlot (Slots (Just a, Just b, Nothing, Nothing, Nothing)) pos = Slots (Just a, Just b, Just pos, Nothing, Nothing)
insertSlot (Slots (Just a, Just b, Just c, Nothing, Nothing)) pos = Slots (Just a, Just b, Just c, Just pos, Nothing)
insertSlot (Slots (Just a, Just b, Just c, Just d, Nothing)) pos = Slots (Just a, Just b, Just c, Just d, Just pos)
insertSlot (Slots (Just b, Just c, Just d, Just e, Just _)) pos = Slots (Just pos, Just b, Just c, Just d, Just e)
insertSlot _ _ = error "bad slot operation"

data Outcome = Outcome
    { oExitedMap :: Bool
    , oCycleDetected :: Bool
    }
    deriving (Show)

update :: State -> (State, Outcome)
update state =
    do
        let oldGrid = stGrid state
        let oldGuardPos = stGuardPos state
        let oldGuardDir = stGuardDir state
        let oldSlots = stGuardSlots state

        let (newGuardPos, newGuardDir, dirChanged, exitedMap) =
                let candidatePos = oldGuardPos .+. oldGuardDir
                 in case Map.lookup candidatePos oldGrid of
                        Nothing -> (oldGuardPos, oldGuardDir, False, True)
                        Just c ->
                            if c `elem` ['#', 'O']
                                then
                                    (oldGuardPos, rotateGuard oldGuardDir, True, False)
                                else
                                    (candidatePos, oldGuardDir, False, False)

        let newSlots =
                bool
                    oldSlots
                    (insertSlot oldSlots newGuardPos)
                    (dirChanged && Just newGuardPos /= firstSlot oldSlots)

        let cycleDetected =
                case newSlots of
                    Slots (Just a, Just b, Just c, Just d, Just e) ->
                        a `elem` [b, c, d, e]
                    _ -> False

        let cycleDetected2 =
                let (height, width) = stGridDims state
                 in stIteration state >= height * width

        -- ~30s
        -- let newGrid :: Map (Int, Int) Char
        --     newGrid =
        --         oldGrid
        --             & Map.update (const $ Just $ guardIcon newGuardDir) newGuardPos
        --             & bool
        --                 (Map.update (const $ Just 'X') oldGuardPos)
        --                 id
        --                 (newGuardPos == oldGuardPos)

        -- ~51s
        let newGrid =
                let updates =
                        if newGuardPos /= oldGuardPos
                            then
                                [ (newGuardPos, guardIcon newGuardDir)
                                , (oldGuardPos, 'X')
                                ]
                            else
                                [(newGuardPos, guardIcon newGuardDir)]
                 in Map.union (Map.fromList updates) oldGrid

        -- ~41s

        ( state
                { stIteration = stIteration state + 1
                , stGuardPos = newGuardPos
                , stGuardDir = newGuardDir
                , stGuardSlots = newSlots
                , stGrid = newGrid
                }
            , Outcome
                { oExitedMap = exitedMap
                , oCycleDetected = cycleDetected || cycleDetected2
                }
            )

countTrail :: Map k Char -> Int
countTrail = Map.foldl' (\tot v -> tot + (if v == 'X' then 1 else 0)) 0

trailPositions :: Map (Int, Int) Char -> [(Int, Int)]
trailPositions = Map.keys . Map.filter (== 'X')

data AdvanceMode
    = SpaceBar
    | SleepMs Int
    deriving (Show)

data VisualMode = VisualMode
    { mapIndex :: Int
    , detectedLoopAt :: [(Int, Int)]
    , advanceMode :: AdvanceMode
    }
    deriving (Show)

pluralize :: (Show a, Eq a, Num a) => a -> String -> String -> String
pluralize n singular plural =
    show n <> " " <> if n == 1 then singular else plural

newtype WantTrailPositions = WantTrailPositions Bool
gameLoop :: Maybe VisualMode -> WantTrailPositions -> State -> IO (Outcome, Maybe [(Int, Int)])
gameLoop visualMode wantTrailPositions state = do
    case visualMode of
        Nothing -> pure ()
        Just (VisualMode{mapIndex, detectedLoopAt, advanceMode}) -> do
            clearFromCursor
            putStrLn $ "Iteration: " ++ show (stIteration state)
            putStrLn $ "Map index: " ++ show mapIndex
            putStrLn $
                mconcat
                    [ "Detected "
                    , pluralize (length detectedLoopAt) "loop" "loops"
                    , " at: " <> show detectedLoopAt
                    ]
            putStrLn ""
            putStr "Guard is at pos: " >> print (stGuardPos state)
            putStr "Guard slots: " >> print (stGuardSlots state)
            putStr "Trail count: " >> print (countTrail $ stGrid state) >> putStrLn ""
            printGridExn state
            case advanceMode of
                SpaceBar -> do
                    c <- getChar
                    print c
                SleepMs ms ->
                    threadDelay $ ms * 1000
    let (newState, outcome) = update state
    let trailPos =
            bool
                Nothing
                (Just $ trailPositions $ stGrid state)
                (coerce wantTrailPositions)
    if oCycleDetected outcome
        then
            putStr "\nCycle detected at trail count of: "
                >> print (countTrail (stGrid state) + 1)
                >> pure (outcome, trailPos)
        else
            if oExitedMap outcome
                then
                    putStr "\nExiting with trail count of: "
                        >> print (countTrail (stGrid state) + 1)
                        >> pure (outcome, trailPos)
                else
                    gameLoop visualMode wantTrailPositions newState

main :: IO ()
main = do
    -- let visualMode =
    --         Just $
    --             VisualMode
    --                 { mapIndex = 0
    --                 , detectedLoopAt = []
    --                 , advanceMode =
    --                     SleepMs 60
    --                     -- SpaceBar
    --                 }

    let visualMode = Nothing
    case visualMode of
        Nothing -> pure ()
        Just _ -> do
            hideCursor
            hSetBuffering stdin NoBuffering -- Enable immediate character reading
            hSetEcho stdin False -- Don't echo characters back

    -- let visualMode = Nothing
    -- example <- readFile "../_inputs/06.example" -- part1=41, part2=6
    example <- readFile "../_inputs/06.example" -- part1=5404, part2=1984 (53s running time, compiled with -O2)
    let gridOrig = makeGrid example

    let guardPos = maybe (error "Guard not found") id (findGuardPos gridOrig)
    let (height, width) =
            let keys = Map.keys gridOrig
             in ( maximum $ fst <$> keys
                , maximum $ snd <$> keys
                )

    let initState newGrid =
            State
                { stIteration = 0
                , stGuardPos = guardPos
                , stGuardDir = (-1, 0)
                , stGuardSlots = Slots (Nothing, Nothing, Nothing, Nothing, Nothing)
                , stGrid = newGrid
                , stGridDims = (height, width)
                }

    (_, mTrailPos) <- gameLoop Nothing (WantTrailPositions True) (initState gridOrig)
    let trailPos = maybe (error "no trail pos!") id mTrailPos
    putStr "TrailPos:" >> print trailPos

    let grids =
            fmap
                ( \pos ->
                    (pos, Map.insert pos 'O' gridOrig)
                )
                trailPos

    putStrLn "Grids to check:"

    let gridsCount = length grids
    (detectedLoopsCount, mapIterations, _) <-
        foldM
            ( \(tot, idx, visualMode_) (oPos, grid) -> do
                putStrLn $ show idx <> "/" <> show gridsCount
                (outcome, _) <- gameLoop visualMode_ (WantTrailPositions False) (initState grid)
                let newVisualMode = case visualMode_ of
                        Nothing -> Nothing
                        Just vm -> Just $ vm{mapIndex = idx}
                if oCycleDetected outcome
                    then
                        pure
                            ( tot + 1
                            , idx + 1
                            , fmap
                                ( \vm ->
                                    vm{detectedLoopAt = oPos : detectedLoopAt vm}
                                )
                                newVisualMode
                            )
                    else pure (tot, idx + 1, newVisualMode)
            )
            ((0, 0, visualMode) :: (Int, Int, Maybe VisualMode))
            grids

    putStrLn "---"
    putStr "detected loops count: " >> print detectedLoopsCount
    putStr "map iterations: " >> print mapIterations

    case visualMode of
        Nothing -> pure ()
        Just _ -> showCursor

    putStrLn "I am done2!"