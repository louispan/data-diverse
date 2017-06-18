{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Data.Diverse.Reduce where

-- | Reduce a polymorphic variant @v xs@ into @r@ using handlers.
-- This class is required in order to step through all the different types in a variant.
class Reduce v handler (xs :: [k]) r where
    reduce :: handler xs r -> v xs -> r
