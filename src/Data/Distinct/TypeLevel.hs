{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel where

import Data.Distinct.TypeLevel.Internal

-- | Get the first index of a type
type IndexOf x (xs :: [*]) = IndexOfEx xs x xs

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [*]) = UnionEx xs '[] xs ~ xs
