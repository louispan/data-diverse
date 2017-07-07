module Main where

import Criterion.Main
import qualified Data.Diverse.ManyBench as ManyBench
import qualified Data.Diverse.WhichBench as WhichBench

-- Our benchmark harness.
main = defaultMain [
    ManyBench.bgroup
  , WhichBench.bgroup
  ]
