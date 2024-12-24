{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wextra #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Day06 where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_, when)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

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

        let newGrid :: Map (Int, Int) Char
            newGrid =
                oldGrid
                    & Map.update (const $ Just $ guardIcon newGuardDir) newGuardPos
                    & bool
                        (Map.update (const $ Just 'X') oldGuardPos)
                        id
                        (newGuardPos == oldGuardPos)

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

data VisualMode = VisualMode
    { mapIndex :: Int
    , detectedLoopAt :: [(Int, Int)]
    }
    deriving (Show)

pluralize :: (Show a, Eq a, Num a) => a -> String -> String -> String
pluralize n singular plural =
    show n <> " " <> if n == 1 then singular else plural

gameLoop :: Maybe VisualMode -> State -> IO Outcome
gameLoop visualMode state = do
    case visualMode of
        Nothing -> pure ()
        Just (VisualMode{mapIndex, detectedLoopAt}) -> do
            putStr "\ESC[2J\ESC[H"
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
            threadDelay 10_000
    let (newState, outcome) = update state
    if oCycleDetected outcome
        then
            putStr "\nCycle detected at trail count of: "
                >> print (countTrail (stGrid state) + 1)
                >> pure outcome
        else
            if oExitedMap outcome
                then
                    putStr "\nExiting with trail count of: "
                        >> print (countTrail (stGrid state) + 1)
                        >> pure outcome
                else
                    gameLoop visualMode newState

main :: IO ()
main = do
    -- let visualMode = Just $ VisualMode{mapIndex = 0, detectedLoopAt = []}
    let visualMode = Nothing
    -- example <- readFile "../_inputs/06.example" -- part1=41, part2=6
    example <- readFile "../_inputs/06.txt" -- part1=5404, part2=1984 (53s running time, compiled with -O2)
    let gridOrig = makeGrid example

    let guardPos = maybe (error "Guard not found") id (findGuardPos gridOrig)
    let (height, width) =
            let keys = Map.keys gridOrig
             in ( maximum $ fst <$> keys
                , maximum $ snd <$> keys
                )

    let grids =
            mapMaybe
                ( \(pos, c) ->
                    bool
                        (Just (pos, Map.insert pos 'O' gridOrig))
                        Nothing
                        (c `elem` ['#', '^'])
                )
                (Map.toList gridOrig)

    -- Detected 11 loops at: [(9,7),(8,7),(8,5),(8,3),(8,1),(7,7),(7,6),(7,2),(7,1),(6,3),(5,6)]
    -- let grids = [((5, 6), Map.insert (5, 6) 'O' gridOrig)]

    putStrLn "Grids to check:"

    -- & Map.insert (6, 3) 'O'
    -- & Map.insert (7, 6) 'O'
    -- & Map.insert (7, 7) 'O'
    -- & Map.insert (8, 1) 'O' -- some doubt here
    -- & Map.insert (8, 3) 'O'
    -- & Map.insert (9, 7) 'O'

    let initState newGrid =
            State
                { stIteration = 0
                , stGuardPos = guardPos
                , stGuardDir = (-1, 0)
                , stGuardSlots = Slots (Nothing, Nothing, Nothing, Nothing, Nothing)
                , stGrid = newGrid
                , stGridDims = (height, width)
                }

    let gridsCount = length grids
    (detectedLoopsCount, mapIterations, _) <-
        foldM
            ( \(tot, idx, visualMode_) (oPos, grid) -> do
                putStrLn $ show idx <> "/" <> show gridsCount
                outcome <- gameLoop visualMode_ (initState grid)
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