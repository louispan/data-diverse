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

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [k]) = DistinctImpl xs xs

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

-- | Get the types at an list of index
type family KindsAtIndices (ns :: [Nat]) (xs :: [k]) :: [k] where
    KindsAtIndices '[] xs = '[]
    KindsAtIndices (n ': ns) xs = KindAtIndex n xs ': KindsAtIndices ns xs

-- | The typelist xs without x. It is okay for x not to exist in xs
type Without x (xs :: [k]) = WithoutImpl x '[] xs

type Reverse (xs :: [k]) = ReverseImpl '[] xs

-- | Index is within Bounds of the typelist
type WithinBounds (n :: Nat) (xs :: [k]) = (n + 1 <= Length xs, 0 <= n)

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

type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
    Concat '[] ys = ys
    Concat (x ': xs) ys = x ': Concat xs ys

type Init (xs :: [k]) = InitImpl '[] xs
