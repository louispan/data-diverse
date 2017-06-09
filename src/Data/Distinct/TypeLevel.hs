{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Distinct.TypeLevel where

import Data.Distinct.TypeLevel.Internal
import Data.Kind
import GHC.TypeLits

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
type family TypesOf x :: [Type] where
    TypesOf () = '[]
    TypesOf (a, b) = '[a, b]
    TypesOf (a, b, c) = '[a, b, c]
    -- declare overlapping instance last in this closed type family
    TypesOf a = '[a]

type family TupleOf (xs :: [Type]) :: Type where
    TupleOf '[] = ()
    TupleOf '[a, b] = (a, b)
    TupleOf '[a, b, c] = (a, b, c)
    -- declare overlapping instance last in this closed type family
    TupleOf '[a] = a

-- | Get the first index of a type
type IndexOf x (xs :: [Type]) = IndexOfEx xs x xs

-- | Get the type at an index
type Index (n :: Nat) (xs :: [Type]) = IndexEx n xs n xs

-- | Constraint: x member of xs
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L257
-- type Member x xs =
--    ( IsMember x xs ~ 'True
--    , x ~ Index (IndexOf x xs) xs
--    , KnownNat (IndexOf x xs)
--    )

type Member x xs =
   ( KnownNat (IndexOf x xs)
   )

-- | Check that a type is member of a type list
type IsMember x xs = IsMemberEx xs x xs
