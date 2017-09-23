{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.Reduce where

-- | Convert something @v@ into @r@ using handlers.
-- This class is required in order to step through all the different types in a variant.
type family Reduced handler
class Reduce v handler where
    reduce :: handler -> v -> Reduced handler
