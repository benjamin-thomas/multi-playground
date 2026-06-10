module Test.Main where

import Prelude

import Effect (Effect)
import Test.Ex01 as Ex01
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Ex01.spec