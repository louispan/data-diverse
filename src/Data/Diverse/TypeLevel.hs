{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}

module Data.Diverse.TypeLevel where

import Data.Diverse.TypeLevel.Internal
import Data.Kind
import GHC.TypeLits

-- | Ensures that @x@ is a unique member of @xs@, and that 'natVal' can be used.
type UniqueMember x xs = (Unique x xs, KnownNat (IndexOf x xs))

-- | Every x in @xs@ is a `UniqueMember x ys`
type family UniqueMembers (xs :: [k]) (ys :: [k]) :: Constraint where
    UniqueMembers '[] ys = ()
    UniqueMembers (x ': xs) ys = (UniqueMember x ys, UniqueMembers xs ys)

-- | Ensures that @x@ is a unique member of @xs@, and that 'natVal' can be used.
type UniqueLabelMember l xs = (UniqueLabel l xs, KnownNat (IndexOf (KindAtLabel l xs) xs))

-- | Ensures that @x@ is a unique member of @xs@ if it exists, and that 'natVal' can be used.
type MaybeUniqueMember x xs = (Unique x xs, KnownNat (PositionOf x xs))

-- | Ensures that @x@ is a member of @xs@ at @n@, and that 'natVal' can be used.
type MemberAt n x xs = (KnownNat n, x ~ KindAtIndex n xs)

-- | Ensures that @x@ is a member of @xs@ at @n@ if it exists, and that 'natVal' can be used.
type MaybeMemberAt n x xs = (KnownNat n, KindAtPositionIs n x xs)

-- | Snoc @x@ to end of @xs@ if @x@ doesn't already exist in @xs@
type family SnocUnique (xs :: [k]) (x :: k) :: [k] where
    SnocUnique '[] x  = '[x]
    SnocUnique (x ': xs) x = x ': xs
    SnocUnique (y ': xs) x = y ': SnocUnique xs x

-- | For each @y@ in @ys@, snocs them to end of @xs@ if @y@ doesn't already exist in @xs@
type family AppendUnique (xs :: [k]) (ys :: [k]) :: [k] where
    AppendUnique '[] xs = xs
    AppendUnique xs '[] = xs
    AppendUnique xs xs = xs
    AppendUnique xs (y ': ys) = AppendUnique (SnocUnique xs y) ys

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

-- | Ensures that the @label@ in @tagged label v@ only ever appears once in @xs@.
type UniqueLabel (l :: k1) (xs :: [k]) = UniqueLabelImpl xs l xs

-- | Ensures that the @label@ list all 'UniqueLabel's
type family UniqueLabels (ls :: [k1]) (xs :: [k]) :: Constraint where
    UniqueLabels '[] xs = ()
    UniqueLabels (l ': ls) xs = (UniqueLabel l xs, UniqueLabels ls xs)

-- | Get the first index of a type (Indexed by 0)
-- Will result in type error if x doesn't exist in xs.
type IndexOf (x :: k) (xs :: [k]) = IndexOfImpl xs x xs

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type PositionOf (x :: k) (xs :: [k]) = PositionOfImpl 0 x xs

-- | Get the type at an index
type KindAtIndex (n :: Nat) (xs :: [k]) = KindAtIndexImpl n xs n xs

-- | Get the type at a label
type KindAtLabel (l :: k1) (xs :: [k]) = KindAtLabelImpl l xs xs

-- | It's actually ok for the position to be zero, but if it's not zero then the types must match
type family KindAtPositionIs (n :: Nat) (x :: k) (xs :: [k]) :: Constraint where
    KindAtPositionIs 0 x xs = ()
    KindAtPositionIs n x xs = (x ~ KindAtIndexImpl (n - 1) xs (n - 1) xs)

-- | Get the types at an list of index
type family KindsAtIndices (ns :: [Nat]) (xs :: [k]) :: [k] where
    KindsAtIndices '[] xs = '[]
    KindsAtIndices (n ': ns) xs = KindAtIndex n xs ': KindsAtIndices ns xs

-- | Get the types with labels @ls@ from @xs@
type family KindsAtLabels (ls :: [k1]) (xs :: [k]) :: [k] where
    KindsAtLabels '[] xs = '[]
    KindsAtLabels (l ': ls) xs = KindAtLabel l xs ': KindsAtLabels ls xs

-- | The typelist @xs@ without first @x@. It is okay for @x@ not to exist in @xs@
type family Remove (x :: k) (xs :: [k]) :: [k] where
    Remove x '[] = '[]
    Remove x (x ': xs) = xs
    Remove x (y ': xs) = y ': Remove x xs

-- | The typelist @xs@ with the first @x@ replaced by @y@. It is okay for @x@ not to exist in @xs@
type Replace (x :: k) (y :: k) (xs :: [k]) = ReplaceImpl x y xs

-- | The typelist @zs@ with the first @xs@ replaced by @ys@.
-- @xs@ must be the same size as @ys@
type Replaces (xs :: [k]) (ys :: [k]) (zs :: [k]) = ReplacesImpl xs ys xs ys zs

-- | The typelist @xs@ without the type at Nat @n@. @n@ must be within bounds of @xs@
type RemoveIndex (n :: Nat) (xs :: [k]) = RemoveIndexImpl n xs n xs

-- | The typelist @xs@ without the type at Nat @n@ replaced by @y@. @n@ must be within bounds of @xs@
type ReplaceIndex (n :: Nat) (y :: k) (xs :: [k]) = ReplaceIndexImpl n xs n y xs

-- | The typelist @xs@ replaced by @ys@ at the indices @ns@. @ns@ and @ys@ must be the same length. @ns@ must be within bounds of @xs@
type ReplacesIndex (ns :: [Nat]) (ys :: [k]) (xs :: [k]) = ReplacesIndexImpl 0 ns ys xs

-- | Returns the typelist up to and excluding @x@. If @x@ doesn't exist, then the original @xs@ is returned.
type family Before (x :: k) (xs :: [k]) :: [k] where
     Before x '[] = '[]
     Before x (x ': xs) = '[]
     Before x (y ': xs) = y ': Before x xs

-- | Returns the typelist up to and including @x@. If @x@ doesn't exist, then the original @xs@ is returned.
type family To (x :: k) (xs :: [k]) :: [k] where
     To x '[] = '[]
     To x (x ': xs) = '[x]
     To x (y ': xs) = y ': To x xs

-- | Returns the typelist after and excluding @x@. If @x@ doesn't exist, then an empty '[] is returned.
type family After (x :: k) (xs :: [k]) :: [k] where
     After x '[] = '[]
     After x (x ': xs) = xs
     After x (y ': xs) = After x xs

-- | Returns the typelist after and including @x@. If @x@ doesn't exist, then an empty '[] is returned.
type family From (x :: k) (xs :: [k]) :: [k] where
     From x '[] = '[]
     From x (x ': xs) = (x ': xs)
     From x (y ': xs) = From x xs

-- | Returns the typelist before (and exluding) index @n@.
-- If @n@ is larger then the @xs@ size, then the original @xs@ is returned.
type family BeforeIndex (n :: Nat) (xs :: [k]) :: [k] where
     BeforeIndex n '[] = '[]
     BeforeIndex 0 xs = '[]
     BeforeIndex n (x ': xs) = x ': BeforeIndex (n - 1) xs

-- | Returns the typelist up to (and including) index @n@.
-- If @n@ is larger then the @xs@ size, then the original @xs@ is returned.
type family ToIndex (n :: Nat) (xs :: [k]) :: [k] where
     ToIndex n '[] = '[]
     ToIndex 0 (x ': xs) = '[x]
     ToIndex n (x ': xs) = x ': ToIndex (n - 1) xs

-- | Returns the typelist after (and exluding) index @n@.
-- If @n@ is larger then the @xs@ size, then an empty '[] is returned.
type family AfterIndex (n :: Nat) (xs :: [k]) :: [k] where
     AfterIndex n '[] = '[]
     AfterIndex 0 (_ ': xs) = xs
     AfterIndex n (x ': xs) = AfterIndex (n - 1) xs

-- | Returns the typelist from (and including) index @n@.
-- If @n@ is larger then the @xs@ size, then an empty '[] is returned.
type family FromIndex (n :: Nat) (xs :: [k]) :: [k] where
     FromIndex n '[] = '[]
     FromIndex 0 xs = xs
     FromIndex n (x ': xs) = FromIndex (n - 1) xs

-- | Get the typelist without the 'Head' type
type family Tail (xs :: [k]) :: [k] where
    Tail '[] = TypeError ('Text "Tail error: empty type list")
    Tail (x ': xs) = xs

-- | Get the first type in a typelist
type family Head (xs :: [k]) :: k where
    Head '[] = TypeError ('Text "Head error: empty type list")
    Head (x ': xs) = x

type family Last (xs :: [k]) :: k where
    Last '[] = TypeError ('Text "Last error: empty type list")
    Last (x ': x' ': xs) = Last (x' ': xs)
    Last '[x] = x

type family Length (xs :: [k]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family NotEmpty (xs :: [k]) :: Constraint where
    NotEmpty '[] = ()
    NotEmpty xs = TypeError ('Text "NotEmpty error: ‘"
                             ':<>: 'ShowType xs
                             ':<>: 'Text "’")

-- | Ensures two typelists are the same length
type SameLength (xs :: [k1]) (ys :: [k2]) = SameLengthImpl xs ys xs ys

-- | Set complement. Returns the set of things in @xs@ that are not in @ys@.
type family Complement (xs :: [k]) (ys :: [k]) :: [k] where
    Complement xs '[] = xs
    Complement xs (y ': ys)  = Complement (Remove y xs) ys

-- | Returns a @xs@ appended with @ys@
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append xs '[] = xs
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

-- | Returns the typelist without the 'Last' type
type family Init (xs :: [k]) :: [k] where
    Init '[]  = TypeError ('Text "Init error: empty type list")
    Init '[x] = '[]
    Init (x ': xs) = x ': Init xs

-- | Takes two lists which must be the same length and returns a list of corresponding pairs.
type Zip (xs :: [k]) (ys :: [k]) = ZipImpl xs ys xs ys

-- | The result from evaluating a 'Case' with a type from a typelist.
type family CaseResult (c ::[k1] -> k2) (x :: k1) :: k2

-- | Return a list of results from applying 'CaseResult' to every type in the @xs@ typelist.
type family CaseResults (c ::[k1] -> k2) (xs :: [k1]) :: [k2] where
    CaseResults c '[] = '[]
    CaseResults c (x ': xs) = CaseResult c x ': CaseResults c xs

-- | Tests if all the types in a typelist satisfy a constraint
type family AllConstrained (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllConstrained c '[] = ()
    AllConstrained c (x ': xs) = (c x, AllConstrained c xs)
-- https://hackage.haskell.org/package/vinyl-0.6.0/docs/Data-Vinyl-TypeLevel.html#t:AllConstrained
