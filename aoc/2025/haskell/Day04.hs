{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use tuple-section" #-}

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

parseLine :: String -> [Bool]
parseLine = map (== '@')

parse :: String -> [[Bool]]
parse =
    map parseLine . lines

coords :: [[a]] -> [((Int, Int), a)]
coords grid =
    concatMap
        ( \(y, row) ->
            map
                (\(x, val) -> ((y, x), val))
                (zip [0 ..] row)
        )
        (zip [0 ..] grid)

neighborsValues' :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
neighborsValues' grid =
    Set.intersection grid . validNeighbors
  where
    validNeighbors :: (Int, Int) -> Set (Int, Int)
    validNeighbors point =
        Set.fromList $
            filter
                (\p -> fst p >= 0 && snd p >= 0)
                (positions point)
{- FOURMOLU_DISABLE -}
    positions (y, x) =
        [ (y-1, x-1), (y-1, x-0), (y-1, x+1)
        , (y-0, x-1),             (y-0, x+1)
        , (y+1, x-1), (y+1, x-0), (y+1, x+1)
        ]
{- FOURMOLU_ENABLE -}

cutoff :: Set (Int, Int) -> (Int, Int) -> Bool
cutoff rolls pos = Set.size (neighborsValues' rolls pos) < 4

{-
λ> part1 <$> readFile "../inputs/Day04.txt"
1351
-}
part1 :: Set (Int, Int) -> Int
part1 rolls =
    Set.size $
        Set.filter cutoff' rolls
  where
    cutoff' = cutoff rolls

part2 :: Set (Int, Int) -> Int
part2 = sum . tryRemove

tryRemove :: Set (Int, Int) -> [Int]
tryRemove curr =
    if removed == 0 then [] else removed : tryRemove next
  where
    removed = Set.size $ Set.difference curr next
    next = Set.filter (not . cutoff curr) curr

tryRemove2 :: Set (Int, Int) -> [Int]
tryRemove2 =
    unfoldr
        ( \curr ->
            let
                removed = Set.size $ Set.difference curr next
                next = Set.filter (not . cutoff curr) curr
             in
                if removed == 0
                    then
                        Nothing
                    else
                        Just (removed, next)
        )

tryRemove3 :: Set (Int, Int) -> [Int]
tryRemove3 rolls =
    takeWhile (> 0) . mapMaybe fst $ aux
  where
    aux :: [(Maybe Int, Set (Int, Int))]
    aux =
        iterate
            ( \(_, curr) ->
                let
                    removed = Set.size $ Set.difference curr next
                    next = Set.filter (not . cutoff curr) curr
                 in
                    (Just removed, next)
            )
            (Nothing, rolls)

makeRolls :: String -> Set (Int, Int)
makeRolls =
    Set.fromList
        . map fst
        . filter snd
        . coords
        . parse

main :: IO ()
main = do
    putStrLn "== Example"
    print . part1 . makeRolls =<< readFile "../inputs/Day04.example"
    print . part2 . makeRolls =<< readFile "../inputs/Day04.example"

    putStrLn "== Real"
    print . part1 . makeRolls =<< readFile "../inputs/Day04.txt"
    print . part2 . makeRolls =<< readFile "../inputs/Day04.txt"

    putStrLn "== Part 2 alternatives"
    print . tryRemove2 . makeRolls =<< readFile "../inputs/Day04.example"
    print . tryRemove3 . makeRolls =<< readFile "../inputs/Day04.example"
