{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel where

import Data.Distinct.TypeLevel.Internal
import GHC.TypeLits

-- | Get the first index of a type
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L219
type family IndexOf a (l :: [*]) :: Nat where
   IndexOf x xs = IndexOfEx xs x xs

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [*]) = UnionEx xs '[] xs ~ xs
