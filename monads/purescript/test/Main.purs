module Test.Main where

import Prelude hiding (add, mul)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
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

  -- `add` and `mul` are more general than `add_verbose`
  assert $
    (Right 12 :: Either String Int) ==
      mul (Right 3) (Right 4)

  assert $
    (Left "First op failed!" :: Either String Int) ==
      mul (Left "First op failed!") (Right 4)

  assert $
    (Left "First op failed!" :: Either String Int) ==
      mul (Left "First op failed!") (Left "Second op failed!")

  assert $
    (Left "Second op failed!" :: Either String Int) ==
      mul (Right 3) (Left "Second op failed!")

