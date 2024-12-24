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

insertSlot :: Slots -> (Int, Int) -> Slots
insertSlot (Slots (Nothing, Nothing, Nothing, Nothing, Nothing)) pos = Slots (Just pos, Nothing, Nothing, Nothing, Nothing)
insertSlot (Slots (a, Nothing, Nothing, Nothing, Nothing)) pos = Slots (a, Just pos, Nothing, Nothing, Nothing)
insertSlot (Slots (a, b, Nothing, Nothing, Nothing)) pos = Slots (a, b, Just pos, Nothing, Nothing)
insertSlot (Slots (a, b, c, d, Nothing)) pos = Slots (a, b, c, d, Just pos)
insertSlot (Slots (b, c, d, e, _)) pos = Slots (Just pos, b, c, d, e)

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
                    dirChanged

        let cycleDetected = case newSlots of
                Slots (Just a, Just b, Just c, Just d, Just e) ->
                    a `elem` [b, c, d, e]
                _ -> False

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
                , oCycleDetected = cycleDetected
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
    putStr "\ESC[2J\ESC[H"
    case visualMode of
        Nothing -> pure ()
        Just (VisualMode{mapIndex, detectedLoopAt}) -> do
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
            threadDelay 1000_000
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
    let visualMode = Just $ VisualMode{mapIndex = 0, detectedLoopAt = []}
    example <- readFile "../_inputs/06.example" -- 41
    -- example <- readFile "../_inputs/06.txt" -- 5404
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

    putStrLn "Grids to check:"

    -- Detected 11 loops at: [(9,7),(8,7),(8,5),(8,3),(8,1),(7,7),(7,6),(7,2),(7,1),(6,3),(4,3)]

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

    total <-
        foldM
            ( \(tot, idx, visualMode_) (oPos, grid) -> do
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
    print total
