{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Reiterate where

import Data.Diverse.Type
import Data.Kind
import GHC.TypeLits

-- | Allows iterating over the types in a typelist
class Reiterate c (xs :: [Type]) where
    -- | Return the next iteration without the 'Head' type x in (x ': xs)
    reiterate :: c xs r -> c (Tail xs) r

-- | Allows iterating over the types in a typelist, whilst also incrementing an Nat index
class ReiterateN c (n :: Nat) (xs :: [Type]) where
    -- | Return the next iteration without the 'Head' type x in (x ': xs)
    reiterateN :: c n xs r -> c (n + 1) (Tail xs) r
