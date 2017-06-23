{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Data.Diverse.Emit where

-- | 'Emit' can generate a value, and is differentiated with an additional @xs@ typelist
class Emit e (xs :: [k]) r where
    emit :: e xs r -> r
