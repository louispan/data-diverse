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
    -- | Return the next iteration without the type x in (x ': xs)
    reiterate :: c xs r -> c (Tail xs) r

-- | Allows iterating over the types in a typelist, using an Nat index
class ReiterateN c (n :: Nat) (xs :: [Type]) where
    -- | Return the next iteration without the type x in (x ': xs)
    reiterateN :: c n xs r -> c (n + 1) (Tail xs) r

-- | Allows iterating over the types in a typelist, using a Labels
class ReiterateL c (ls :: [k]) (xs :: [Type]) where
    -- | Return the next iteration without the type x in (x ': xs)
    reiterateL :: c ls xs r -> c (Tail ls) (Tail xs) r
