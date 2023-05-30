module Test.Main where

import Prelude hiding (add, mul)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Monads (add_verbose, add, mul)
import Test.Assert (assert)

-- spago test -wl

main :: Effect Unit
main = do
  assert $
    Just 3 ==
      add_verbose (Just 1) (Just 2)

  assert $
    Nothing ==
      add_verbose Nothing (Just 2)

  assert $
    Nothing ==
      add_verbose (Just 1) Nothing

  assert $
    (Just 3) ==
      add (Just 1) (Just 2)

  assert $
    Nothing ==
      add Nothing (Just 2)

  assert $
    Nothing ==
      add (Just 1) Nothing

  assert $
    Just 12 ==
      mul (Just 3) (Just 4)

  assert $
    Nothing ==
      mul Nothing (Just 4)

  assert $
    Nothing ==
      mul (Just 3) Nothing

