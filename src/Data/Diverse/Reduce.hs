{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.Reduce where

-- | Convert something @v@ into @r@ using handlers.
-- This class is required in order to step through all the different types in a variant.
class Reduce v handler r where
    reduce :: handler r -> v -> r
