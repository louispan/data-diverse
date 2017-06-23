{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}

module Data.Diverse.Type where

import Data.Diverse.Type.Internal
import Data.Kind
import GHC.TypeLits

-- FIXME: Bring back Member, so we can have functions for polymorhpic records

-- FIXME: lower IsDistinct constraints to just have that type be distinct, not all typelist

-- | A constraint ensuring that the type list contain unique types
type IsDistinct (xs :: [k]) = IsDistinctImpl xs xs

type family Distinct (xs :: [k]) :: [k] where
    Distinct '[] = '[]
    Distinct (x ': xs) = DistinctImpl xs x xs

type Unique x (xs :: [k]) = UniqueImpl xs x xs

-- | Gets the result type from an list of handlers/continuations of different types.
type family OutcomeOf (xs :: [Type]) :: Type where
    OutcomeOf '[] = TypeError ('Text "No continuation found in empty type list")
    OutcomeOf ((a -> r) ': xs) = OutcomeOfImpl ((a -> r) ': xs) r xs
    OutcomeOf ctx = TypeError ('Text "No continuation found in head of "
                               ':<>: 'Text "‘"
                               ':<>: 'ShowType ctx
                               ':<>: 'Text "’")

-- | Get the first index of a type (Indexed by 0)
-- Will result in type error if x doesn't exist in xs.
type IndexOf x (xs :: [k]) = IndexOfImpl xs x xs

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type PositionOf x (xs :: [k]) = PositionOfImpl 0 x xs

-- | Get the type at an index
type KindAtIndex (n :: Nat) (xs :: [k]) = KindAtIndexImpl n xs n xs

-- | It's actually ok for the position to be zero, but if it's not zero then the types must match
type family KindAtPositionIs (x :: k) (n :: Nat) (xs :: [k]) :: Constraint where
    KindAtPositionIs x 0 xs = ()
    KindAtPositionIs x n xs = (x ~ KindAtIndexImpl (n - 1) xs (n - 1) xs)

-- | Get the types at an list of index
type family KindsAtIndices (ns :: [Nat]) (xs :: [k]) :: [k] where
    KindsAtIndices '[] xs = '[]
    KindsAtIndices (n ': ns) xs = KindAtIndex n xs ': KindsAtIndices ns xs

-- | The typelist xs without x. It is okay for x not to exist in xs
type family Without x (xs :: [k]) :: [k] where
    Without x '[] = '[]
    Without x (x ': xs) = Without x xs
    Without x (y ': xs) = y ': Without x xs

-- | Index is within Bounds of the typelist
type WithinBounds (n :: Nat) (xs :: [k]) = (n + 1 <= Length xs, 0 <= n)

type family NotEmpty (xs :: [k]) :: Constraint where
    NotEmpty '[] = TypeError ('Text "Typelist can't be empty")
    NotEmpty (x ': xs) = ()

type family Length (xs :: [k]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Tail (xs :: [k]) :: [k] where
    Tail '[] = TypeError ('Text "Cannot Tail an empty type list")
    Tail (x ': xs) = xs

type family Head (xs :: [k]) :: k where
    Head '[] = TypeError ('Text "Cannot Head an empty type list")
    Head (x ': xs) = x

type family Last (xs :: [k]) :: k where
    Last '[] = TypeError ('Text "Cannot Last an empty type list")
    Last (x ': x' ': xs) = Last (x' ': xs)
    Last '[x] = x

type SameLength (xs :: [k1]) (ys :: [k2]) = SameLengthImpl xs ys xs ys

-- | Set complement. Returns the set of things in xs that are not in ys.
type family Complement (xs :: [k]) (ys :: [k]) :: [k] where
    Complement xs '[] = xs
    Complement xs (y ': ys)  = Complement (Without y xs) ys

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type family Init (xs :: [k]) :: [k] where
    Init '[]  = TypeError ('Text "Cannot Init an empty type list")
    Init '[x] = '[]
    Init (x ': xs) = x ': Init xs
