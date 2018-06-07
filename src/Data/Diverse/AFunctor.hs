{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.AFunctor where

import Data.Diverse.TypeLevel

-- | Given a 'Data.Diverse.Case' that transforms each type in the
-- typelist, convert a @f xs@ to @f (CasesResults c xs)@
class AFunctor f c xs where
    afmap :: c xs -> f xs -> f (CaseResults c xs)
