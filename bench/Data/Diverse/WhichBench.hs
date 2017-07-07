{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.Diverse.WhichBench where

import qualified Criterion.Main as C
import Data.Diverse

bgroup = C.bgroup "Which"
    [ C.bench "read" $ C.whnf (read @(Which '[Int, Bool])) "pickN @0 Proxy 5"
    ]
