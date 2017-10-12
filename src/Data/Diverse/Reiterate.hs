{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Reiterate where

import Data.Diverse.TypeLevel
import Data.Kind
import GHC.TypeLits

-- | Allows iterating over the types in a typelist
class Reiterate c (xs :: [Type]) where
    -- | Return the next iteration without the 'Head' type x in (x ': xs)
    reiterate :: c xs -> c (Tail xs)

-- | Allows iterating over the types in a typelist, whilst also incrementing an Nat index
class ReiterateN c (n :: Nat) (xs :: [Type]) where
    -- | Return the next iteration without the 'Head' type x in (x ': xs)
    reiterateN :: c n xs -> c (n + 1) (Tail xs)
