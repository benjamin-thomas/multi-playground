module Test.Ex01 (spec) where

import Prelude

import Ex01 as Ex01

import Data.List (fromFoldable, List(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Ex01.last" do
  it "returns the last element" do
    Ex01.last (fromFoldable [ "a", "b", "c", "d" ]) `shouldEqual` Just "d"

  it "returns Nothing for empty list" do
    Ex01.last (Nil :: List String) `shouldEqual` Nothing