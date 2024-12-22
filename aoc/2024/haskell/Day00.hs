{-# OPTIONS_GHC -Wall -Wextra #-}

{- cabal:

build-depends: base
-}

{-
NOTE: the pretty printer requires a minimal cabal script.
      If you do require dependencies, just create a normal cabal project, as the
      vscode plugin is not aware of cabal scripts dependencies.
 -}

{-

Terminal 1
==========
./repl
> :cmd return $ unlines [":!clear",":reload", "answer1 example"]

Terminal 2
==========
find *.hs | entr tmux send-keys -t aoc:0 Up Enter

Terminal 2 (alternative)
========================
find *.hs | entr tmux send-keys -t aoc:0 ':cmd return $ unlines [":!clear",":reload"]' Enter

Or just do this
===============
ghcid -T :main ./Day00.hs

 -}

module Day00 (main, example) where

main :: IO ()
main = do
    input <- readFile "../_inputs/00.txt"
    print $ answer1 input
    print $ answer2 input

example :: String
example =
    unlines
        []

answer1 :: String -> Int
answer1 = const 0

answer2 :: String -> Int
answer2 =
    const 0