{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.TypeLevel where

import Data.Distinct.TypeLevel.Internal
import Data.Kind

-- | Get the first index of a type
type IndexOf x (xs :: [Type]) = IndexOfEx xs x xs

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [Type]) = UnionEx xs '[] xs ~ xs

-- -- | Convert a list types into a list of handlers/continuations of that type.
-- type family Accepts (xs :: [Type]) r :: [Type] where
--     Accepts '[] r = '[]
--     Accepts (x ': xs) r = (x -> r) ': Accepts xs r

-- type family TupleOf (xs :: [Type]) :: Type where
--     TupleOf '[] = ()
--     TupleOf '[a, b] = (a, b)
--     TupleOf '[a, b, c] = (a, b, c)
--     -- declare overlapping instance last in this closed type family
--     TupleOf '[a] = a
