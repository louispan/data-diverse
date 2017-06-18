{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Diverse.Reiterate where

import Data.Diverse.Type

-- | Allows iterating over the types in a typelist
class Reiterate c (xs :: [k]) where
    -- | Return the next iteration without the type x in (x ': xs)
    reiterate :: c xs r -> c (Tail xs) r
