{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.ATraversable where

import Data.Diverse.TypeLevel

-- | Given a 'Data.Diverse.Case' that transforms each type in the typelist within an
-- 'Applicative' context @m@, convert a @f xs@ to @m (f ('TraverseResults' c m xs))@,
-- where @('TraverseResults' c m xs)@ corresponds to @('CaseResults' (c m) xs)@ with the
-- @m@ layer peeled off from each result.
--
-- This is primarily meant to be used with 'Data.Diverse.Case.Case's from the
-- "Data.Diverse.CaseIxed" module.
class ATraversable f c m xs where
    atraverse
        :: ( Applicative m
           -- Throws a type error when the 'Case' is stuck
           -- (most likely because the kind does not match).
           , IsTraversalCase c
           -- Defers the evaluation of the traversal results, to avoid getting another
           -- (confusing) type error when the 'Case' is stuck.
           , xs' ~ (TraverseResults c m xs)
           )
        => c m xs
        -> f xs
        -> m (f xs')
