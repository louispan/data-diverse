{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Data.Diverse.ManyBench where

import qualified Criterion.Main as C
import Data.Diverse

bgroup = C.bgroup "Many"
    [ C.bench "read" (C.whnf (read @(Many '[Int, Bool, Char, Maybe Char])) "5 ./ False ./ 'X' ./ Just 'O' ./ nil")
    ]
