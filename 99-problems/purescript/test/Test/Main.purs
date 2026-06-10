module Test.Main where

import Prelude

import Effect (Effect)
import Test.Ex01 as Ex01
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

{-

Run the tests with:

rg --files | entr -c spago test

 -}

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Ex01.spec