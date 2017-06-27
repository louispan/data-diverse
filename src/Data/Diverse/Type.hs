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

-- | Ensures that @x@ is a unique member of @xs@, and that 'natVal' can be used.
type UniqueMember x xs = (Unique x xs, KnownNat (IndexOf x xs))

-- | Ensures that @x@ is a unique member of @xs@ if it exists, and that 'natVal' can be used.
type MaybeUniqueMember x xs = (Unique x xs, KnownNat (PositionOf x xs))

-- | Ensures that @x@ is a member of @xs@ at @n@, and that 'natVal' can be used.
type MemberAt n x xs = (KnownNat n, x ~ KindAtIndex n xs)

-- | Ensures that @x@ is a member of @xs@ at @n@ if it exists, and that 'natVal' can be used.
type MaybeMemberAt n x xs = (KnownNat n, KindAtPositionIs n x xs)

-- | Ensures x is a unique member in @xs@ iff it exists in @ys@
type family UniqueIfExists ys x xs :: Constraint where
    UniqueIfExists '[] x xs = ()
    UniqueIfExists (y ': ys) y xs = Unique y xs
    UniqueIfExists (y ': ys) x xs = UniqueIfExists ys x xs

-- | Ensures that the type list contain unique types
type IsDistinct (xs :: [k]) = IsDistinctImpl xs xs

-- | Return the list of distinct types in a typelist
type family Nub (xs :: [k]) :: [k] where
    Nub '[] = '[]
    Nub (x ': xs) = NubImpl xs x xs

-- | Ensures that @x@ only ever appears once in @xs@
type Unique (x :: k) (xs :: [k]) = UniqueImpl xs x xs

-- | Get the first index of a type (Indexed by 0)
-- Will result in type error if x doesn't exist in xs.
type IndexOf (x :: k) (xs :: [k]) = IndexOfImpl xs x xs

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type PositionOf (x :: k) (xs :: [k]) = PositionOfImpl 0 x xs

-- | Get the type at an index
type KindAtIndex (n :: Nat) (xs :: [k]) = KindAtIndexImpl n xs n xs

-- | Get the type at a label
type KindAtLabel (l :: k1) (xs :: [k2]) = KindAtLabelImpl l xs xs

-- | It's actually ok for the position to be zero, but if it's not zero then the types must match
type family KindAtPositionIs (n :: Nat) (x :: k) (xs :: [k]) :: Constraint where
    KindAtPositionIs 0 x xs = ()
    KindAtPositionIs n x xs = (x ~ KindAtIndexImpl (n - 1) xs (n - 1) xs)

-- | Get the types at an list of index
type family KindsAtIndices (ns :: [Nat]) (xs :: [k]) :: [k] where
    KindsAtIndices '[] xs = '[]
    KindsAtIndices (n ': ns) xs = KindAtIndex n xs ': KindsAtIndices ns xs

-- | The typelist @xs@ without first @x@. It is okay for @x@ not to exist in @xs@
type family Without (x :: k) (xs :: [k]) :: [k] where
    Without x '[] = '[]
    Without x (x ': xs) = xs
    Without x (y ': xs) = y ': Without x xs

-- | The typelist @xs@ with the first @x@ replaced by @y@. It is okay for @x@ not to exist in @xs@
type Replace (x :: k) (y :: k) (xs :: [k]) = ReplaceImpl x y xs

-- | The typelist @zs@ with the first @xs@ replaced by @ys@.
-- @xs@ must be the same size as @ys@
type Replaces (xs :: [k]) (ys :: [k]) (zs :: [k]) = ReplacesImpl xs ys xs ys zs

-- | The typelist @xs@ without the type at Nat @n@. @n@ must be within bounds of @xs@
type WithoutIndex (n :: Nat) (xs :: [k]) = WithoutIndexImpl n xs n xs

-- | The typelist @xs@ without the type at Nat @n@ replaced by @y@. @n@ must be within bounds of @xs@
type ReplaceIndex (n :: Nat) (y :: k) (xs :: [k]) = ReplaceIndexImpl n xs n y xs

-- | The typelist @xs@ replaced by @ys@ at the indices @ns@. @ns@ and @ys@ must be the same length. @ns@ must be within bounds of @xs@
type ReplacesIndex (ns :: [Nat]) (ys :: [k]) (xs :: [k]) = ReplacesIndexImpl 0 ns ys xs

-- | Gets the ength of a typelist
type family Length (xs :: [k]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

-- | Get the typelist without the 'Head' type
type family Tail (xs :: [k]) :: [k] where
    Tail '[] = TypeError ('Text "Tail error: empty type list")
    Tail (x ': xs) = xs

-- | Get the first type in a typelist
type family Head (xs :: [k]) :: k where
    Head '[] = TypeError ('Text "Head error: empty type list")
    Head (x ': xs) = x

-- | Get the last type in a typelist
type family Last (xs :: [k]) :: k where
    Last '[] = TypeError ('Text "Last error: empty type list")
    Last (x ': x' ': xs) = Last (x' ': xs)
    Last '[x] = x

-- | Ensures two typelists are the same length
type SameLength (xs :: [k1]) (ys :: [k2]) = SameLengthImpl xs ys xs ys

-- | Set complement. Returns the set of things in @xs@ that are not in @ys@.
type family Complement (xs :: [k]) (ys :: [k]) :: [k] where
    Complement xs '[] = xs
    Complement xs (y ': ys)  = Complement (Without y xs) ys

-- | Returns a @xs@ appended with @ys@
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

-- | Returns the typelist without the 'Last' type
type family Init (xs :: [k]) :: [k] where
    Init '[]  = TypeError ('Text "Init error: empty type list")
    Init '[x] = '[]
    Init (x ': xs) = x ': Init xs

-- | Takes two lists which must be the same length and returns a list of corresponding pairs.
type Zip (xs :: [k]) (ys :: [k]) = ZipImpl xs ys xs ys
