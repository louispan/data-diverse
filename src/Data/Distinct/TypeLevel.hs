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

-- | Check that a list is a subset of another
type family IsSubset smaller larger :: Bool where
   IsSubset s l = IsSubsetEx l s l

-- -- | Convert a list types into a list of handlers/continuations with a result type.
-- type family Accepts r (xs :: [Type]) :: [Type] where
--     Accepts r '[] = '[]
--     Accepts r (x ': xs) = (x -> r) ': Accepts r xs

-- -- | Gets the result type from an list of handler/continuations of different types.
-- type family SwitchResult (xs :: [Type]) :: Type where
--     SwitchResult '[] = TypeError ( 'Text "No continuation found in empty type list")
--     SwitchResult ((a -> r) ': xs) = SwitchResultEx ((a -> r) ': xs) r xs
--     SwitchResult ctx = TypeError ( 'Text "No continuation found in head of "
--                                     ':<>: 'Text "‘"
--                                     ':<>: 'ShowType ctx
--                                     ':<>: 'Text "’")


     -- type family TupleOf (xs :: [Type]) :: Type where
--     TupleOf '[] = ()
--     TupleOf '[a, b] = (a, b)
--     TupleOf '[a, b, c] = (a, b, c)
--     -- declare overlapping instance last in this closed type family
--     TupleOf '[a] = a
