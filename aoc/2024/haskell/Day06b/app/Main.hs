{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

{-# HLINT ignore "Redundant pure" #-}

{-
NOTE: although I could "execute" the file due to the shebang, "cabal run" is
      still required due to the injection of the extra ghc options.
      Also, having that shebang seems to make the vscode extension happy.

Find the cause of an exception with:
   cabal run ./Day06_alt.hs --ghc-options "-main-is Day06_alt -prof -fprof-auto" -- +RTS -xc

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

Or simply run with:
  cabal run ./Day06_alt.hs --ghc-options "-main-is Day06_alt"

 -}

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, when)
import Data.Array.ST (inRange, runSTUArray, thaw, writeArray)
import Data.Array.Unboxed (IArray (bounds), UArray, array, assocs, elems, (!), (//))
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stderr, stdin, stdout)

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

{-

Create a 1D array:
>>> array (0,2) [(0, 'A'), (1, 'B'), (2, 'C')]
array (0,2) [(0,'A'),(1,'B'),(2,'C')]

---

Create a 2D array:
>>> array ((0,0),(2,2)) [((0,0), 'A'), ((0,1), 'B'), ((0,2), 'C'), ((1,0), 'D'), ((1,1), 'E'), ((1,2), 'F'), ((2,0), 'G'), ((2,1), 'H'), ((2,2), 'I')]
array ((0,0),(2,2)) [((0,0),'A'),((0,1),'B'),((0,2),'C'),((1,0),'D'),((1,1),'E'),((1,2),'F'),((2,0),'G'),((2,1),'H'),((2,2),'I')]

 -}
makeGrid :: String -> UArray (Int, Int) Char
makeGrid input =
    let len = length (head (lines input)) - 1
     in array
            ((0, 0), (len, len))
            [ ((y, x), c)
            | (y, line) <- zip [0 ..] (lines input)
            , (x, c) <- zip [0 ..] line
            ]

{- FOURMOLU_DISABLE -}
{-

Debug with:

$ ghci
> :set -fbreak-on-error
> :trace main
> :back
> :list

 -}
{- FOURMOLU_ENABLE -}

findGuardPos :: UArray (Int, Int) Char -> Maybe (Int, Int)
findGuardPos arr =
    foldr
        (\(coord, v) acc -> if v == '^' then Just coord else acc)
        Nothing
        (assocs arr)

lookupExn :: (HasCallStack) => UArray (Int, Int) Char -> (Int, Int) -> Char
lookupExn grid (y, x) =
    grid ! (y, x)

printGridExn :: UArray (Int, Int) Char -> IO ()
printGridExn grid = do
    let ((minRow, minCol), (maxRow, maxCol)) = bounds grid
    forM_ [minRow .. maxRow] $ \y -> do
        forM_ [minCol .. maxCol] $ \x -> do
            let char = lookupExn grid (y, x)
            when (char == '^' || char == 'v' || char == '>' || char == '<') $ putStr yellow
            when (char == 'X') $ putStr gray
            putChar char
            putStr reset
        putStrLn ""

data State = State
    { stIteration :: Int
    , stGuardPos :: (Int, Int)
    , stGuardDir :: (Int, Int)
    , stGrid :: UArray (Int, Int) Char
    , stVisited :: Set ((Int, Int), (Int, Int))
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

data Outcome = Outcome
    { oExitedMap :: Bool
    , oCycleDetected :: Bool
    }
    deriving (Show)

search :: (HasCallStack) => UArray (Int, Int) Char -> (Int, Int) -> Maybe Char
search grid pos =
    if inRange (bounds grid) pos
        then Just $ grid ! pos
        else Nothing

update :: State -> (State, Outcome)
update state =
    do
        let oldGrid = stGrid state
        let oldGuardPos = stGuardPos state
        let oldGuardDir = stGuardDir state
        let oldVisited = stVisited state

        let (newGuardPos, newGuardDir, changedDir, exitedMap) =
                let candidatePos = oldGuardPos .+. oldGuardDir
                 in case search oldGrid candidatePos of
                        Nothing -> (oldGuardPos, oldGuardDir, False, True)
                        Just c ->
                            if c `elem` ['#', 'O']
                                then
                                    (oldGuardPos, rotateGuard oldGuardDir, True, False)
                                else
                                    (candidatePos, oldGuardDir, False, False)

        let cycleDetected =
                Set.member (newGuardPos, newGuardDir) oldVisited

        let newVisited =
                if not changedDir
                    then oldVisited
                    else
                        Set.insert (newGuardPos, newGuardDir) oldVisited

        -- ~30s
        -- let newGrid :: Map (Int, Int) Char
        --     newGrid =
        --         oldGrid
        --             & Map.update (const $ Just $ guardIcon newGuardDir) newGuardPos
        --             & bool
        --                 (Map.update (const $ Just 'X') oldGuardPos)
        --                 id
        --                 (newGuardPos == oldGuardPos)

        -- 30s here
        -- let newGrid :: UArray (Int, Int) Char
        --     newGrid =
        --         let updates =
        --                 if newGuardPos /= oldGuardPos
        --                     then
        --                         [ (newGuardPos, guardIcon newGuardDir)
        --                         , (oldGuardPos, 'X')
        --                         ]
        --                     else
        --                         [(newGuardPos, guardIcon newGuardDir)]
        --          in oldGrid // updates

        -- 30s here
        let newGrid :: UArray (Int, Int) Char
            newGrid = runSTUArray $ do
                mutGrid <- thaw oldGrid

                writeArray mutGrid newGuardPos (guardIcon newGuardDir)
                when (newGuardPos /= oldGuardPos) $ writeArray mutGrid oldGuardPos 'X'

                pure mutGrid

        -- ~41s

        ( state
                { stIteration = stIteration state + 1
                , stGuardPos = newGuardPos
                , stGuardDir = newGuardDir
                , stGrid = newGrid
                , stVisited = newVisited
                }
            , Outcome
                { oExitedMap = exitedMap
                , oCycleDetected = cycleDetected
                }
            )

countTrail :: UArray (Int, Int) Char -> Int
countTrail arr =
    foldr (\v tot -> tot + (if v == 'X' then 1 else 0)) 0 (elems arr)

trailPositions :: UArray (Int, Int) Char -> [(Int, Int)]
trailPositions arr =
    let kv = assocs arr
     in fst <$> filter ((==) 'X' . snd) kv

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
gameLoop :: (HasCallStack) => Maybe VisualMode -> WantTrailPositions -> State -> IO (Outcome, Maybe [(Int, Int)])
gameLoop visualMode wantTrailPositions state = do
    case visualMode of
        Nothing -> do
            pure ()
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
            putStr "Trail count: " >> print (countTrail $ stGrid state) >> putStrLn ""
            printGridExn (stGrid state)
            putStr "\n\nVisited: " >> print (stVisited state)
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

main :: (HasCallStack) => IO ()
main = do
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering
    let visualMode = Nothing
    -- let visualMode =
    --         Just $
    --             VisualMode
    --                 { mapIndex = 0
    --                 , detectedLoopAt = []
    --                 , advanceMode =
    --                     -- SleepMs 60
    --                     SpaceBar
    --                 }

    case visualMode of
        Nothing -> pure ()
        Just _ -> do
            hideCursor
            hSetBuffering stdin NoBuffering -- Enable immediate character reading
            hSetEcho stdin False -- Don't echo characters back
            -- let visualMode = Nothing
            -- example <- readFile "../_inputs/06.example" -- part1=41, part2=6
    example <- readFile "../../_inputs/06.txt" -- part1=5404, part2=1984 (53s running time, compiled with -O2)
    let gridOrig :: UArray (Int, Int) Char
        gridOrig = makeGrid example

    let guardPos = maybe (error "Guard not found") id (findGuardPos gridOrig)
    putStrLn "Guard pos:" >> print guardPos

    let initState newGrid =
            State
                { stIteration = 0
                , stGuardPos = guardPos
                , stGuardDir = (-1, 0)
                , stGrid = newGrid
                , stVisited = Set.empty
                }

    (_, mTrailPos) <- gameLoop Nothing (WantTrailPositions True) (initState gridOrig)
    let trailPos = maybe (error "no trail pos!") id mTrailPos
    putStr "TrailPos:" >> print trailPos

    let grids :: [((Int, Int), UArray (Int, Int) Char)]
        grids =
            fmap
                ( \pos ->
                    (pos, gridOrig // [(pos, 'O')])
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
