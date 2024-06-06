module Main
  ( handlePaths
  , main
  ) where

import Prelude

import Data.Array (drop, last, length, uncons)
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Node.Encoding as E

{-

rg --files | entr -c spago run --exec-args="../test.txt /usr/share/dict/*"

spago bundle-app --platform=node --main Main --to dist/index.js
time node dist/index.js /usr/share/dict/*

purs-tidy format-in-place ./src/Main.purs

 -}

type Counters =
  { bytes :: Int
  , runes :: Int
  , lines :: Int
  , words :: Int
  }

handleContents :: String -> Counters
handleContents str =
  let
    -- NOTE: words is wrong, not sure why
    bytes = E.byteLength str E.UTF8
    runes = S.length str
    lines = (length $ split (Pattern "\n") str) - 1
    words = length $ split (Pattern " ") str
  in
    { bytes, runes, lines, words }

handlePaths :: Array String -> Effect Unit
handlePaths paths =
  case uncons paths of
    Nothing -> pure unit
    Just { head, tail } -> do
      contents <- readTextFile E.UTF8 head
      let counters = handleContents contents
      let basename = last $ split (Pattern "/") head
      log $ show $ Tuple basename counters
      handlePaths tail

main :: Effect Unit
main = do
  paths <- drop 2 <$> argv
  handlePaths paths
  log "Finished!"
