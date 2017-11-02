module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import SimpleStorageSpec (simpleStorageSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')

main = run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] $ do
  simpleStorageSpec
