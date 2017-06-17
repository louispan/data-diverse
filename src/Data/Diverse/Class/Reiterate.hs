{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Diverse.Class.Reiterate where

import Data.Diverse.Type
import Data.Kind

-- | Allows iterating over the types in a typelist by
class Reiterate c (xs :: [Type]) where
    -- | Return the next iteration without the type x in (x ': xs)
    reiterate :: c xs r -> c (Tail xs) r
