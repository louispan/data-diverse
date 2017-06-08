{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.TypeLevel where

import Data.Distinct.TypeLevel.Internal
import Data.Kind
import GHC.TypeLits

-- | Get the first index of a type
type IndexOf x (xs :: [Type]) = IndexOfEx xs x xs

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [Type]) = UnionEx xs '[] xs ~ xs

-- | Convert a list types into a list of handlers/continuations of that type.
type family Accepts (xs :: [Type]) r :: [Type] where
    Accepts '[] r = '[]
    Accepts (x ': xs) r = (x -> r) ': Accepts xs r

type family AcceptResult (xs :: [Type]) :: Type where
    AcceptResult '[] = TypeError ( 'Text "Empty type list cannot have an AcceptResult")
    AcceptResult ((a -> r) ': xs) = AcceptResultEx ((a -> r) ': xs) r xs
    AcceptResult ctx = TypeError ( 'Text " All types in"
                                    ':<>: 'Text "‘"
                                    ':<>: 'ShowType ctx
                                    ':<>: 'Text "’"
                                    ':<>: 'Text " do not result in the same type")


     -- type family TupleOf (xs :: [Type]) :: Type where
--     TupleOf '[] = ()
--     TupleOf '[a, b] = (a, b)
--     TupleOf '[a, b, c] = (a, b, c)
--     -- declare overlapping instance last in this closed type family
--     TupleOf '[a] = a
