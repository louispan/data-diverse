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

-- | Add a type to a typelist, disallowing duplicates.
-- NB. xs are not checked.
type Insert (y :: Type) (xs :: [Type]) = InsertImpl xs y xs

-- | Combine two type lists together, assuming disallowing duplicates from ys
-- NB. xs are not checked.
type family Union (xs :: [Type]) (ys :: [Type]) :: [Type] where
    -- empty case
    Union '[] '[] = '[]
    Union xs '[] = xs
    Union xs (y ': ys) = Union (Insert y xs) ys

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [Type]) = Union '[] xs ~ xs

-- | Convert a list types into a list of handlers/continuations with a result type.
-- type family Accepts r (xs :: [Type]) :: [Type] where
--     Accepts r '[] = '[]
--     Accepts r (x ': xs) = (x -> r) ': Accepts r xs

-- | Gets the result type from an list of handlers/continuations of different types.
type family OutcomeOf (xs :: [Type]) :: Type where
    OutcomeOf '[] = TypeError ('Text "No continuation found in empty type list")
    OutcomeOf ((a -> r) ': xs) = OutcomeOfImpl ((a -> r) ': xs) r xs
    OutcomeOf ctx = TypeError ('Text "No continuation found in head of "
                                    ':<>: 'Text "‘"
                                    ':<>: 'ShowType ctx
                                    ':<>: 'Text "’")

type family TypesOfNotSupported t :: [Type] where
    TypesOfNotSupported t = TypeError ('Text "TypesOf ‘"
                                    ':<>: 'ShowType t
                                    ':<>: 'Text "’ is too large to be suppored")

type family TupleOfNotSupported (xs :: [Type]) :: Type where
    TupleOfNotSupported xs = TypeError ('Text "TupleOf ‘"
                                    ':<>: 'ShowType xs
                                    ':<>: 'Text "’ is too large to be suppored")

-- | Get the type list of out a tuple
-- The size of this should be synchronized withe the number of 'Has' instances in Catalog
type family TypesOf x :: [Type] where
    TypesOf () = '[]
    TypesOf (a, b) = '[a, b]
    TypesOf (a, b, c) = '[a, b, c]
    TypesOf (a, b, c, d) = TypesOfNotSupported (a, b, c, d)
    -- TypesOf (a, b, c) = '[a, b, c]
    -- declare overlapping instance last in this closed type family
    TypesOf a = '[a]

-- | Get the tuple with equivalent type list
-- The size of this should be synchronized withe the number of 'Has' instances in Catalog
type family TupleOf (xs :: [Type]) :: Type where
    TupleOf '[] = ()
    TupleOf '[a, b] = (a, b)
    TupleOf '[a, b, c] = (a, b, c)
    TupleOf '[a, b, c, d] = TupleOfNotSupported [a, b, c, d]
    -- declare overlapping instance last in this closed type family
    TupleOf '[a] = a

-- | Get the first index of a type (Indexed by 0)
-- Will result in type error if x doesn't exist in xs.
type IndexOf x (xs :: [Type]) = IndexOfImpl xs x xs

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type PositionOf x (xs :: [Type]) = PositionOfImpl 0 x xs

-- | Get the type at an index
type TypeAt (n :: Nat) (xs :: [Type]) = TypeAtImpl n xs n xs

-- | Constraint: x member of xs
-- https://github.com/haskus/haskus-utils/blob/3b6bd1c3fce463173b9827b579fb95c911e5a806/src/lib/Haskus/Utils/Types/List.hs#L257
-- type Member x xs =
--    ( IsMember x xs ~ 'True
--    , x ~ TypeAt (IndexOf x xs) xs
--    , KnownNat (IndexOf x xs)
--    )

-- | The typelist xs without x. It is okay for x not to exist in xs
type Without x (xs :: [Type]) = WithoutImpl x '[] xs

type Reverse (xs :: [Type]) = ReverseImpl '[] xs

-- | KnownNat constraint is proof to GHC that it can instantiate a value of KnownNat
-- for a particular typelevel Nat for a particular usage of 'natVal'.
-- 'Member' is required for using 'natVal' for a particular index (starting from 0)
-- of a type in a typelist.
type Member x xs = (KnownNat (IndexOf x xs))

-- | KnownNat constraint is proof to GHC that it can instantiate a value of KnownNat
-- for a particular typelevel Nat for a particular usage of 'natVal'.
-- MaybeMember required to use 'natVal' for a particular position (starting from 1)
-- of a type in a typelist, or 0 if the type is not found.
type MaybeMember x xs = KnownNat (PositionOf x xs)

-- | For all x in xs, provide a proof that there is a KnownNat of x in ys.
type family MembersOf (ctx :: [Type]) (xs :: [Type]) :: Constraint where
    MembersOf ctx '[] = ()
    MembersOf ctx (x ': xs) = (Member x ctx,  MembersOf ctx xs)

-- -- | For each x in xs, create a (KnownNat (IndexOf x ys)) contraint
-- type family Subset (smaller :: [Type]) (larger :: [Type]) (xs :: [Type]) :: Constraint where
--     Subset as bs  '[] = ()
--     Subset as bs (x ': xs) = (Member2 x as bs, Subset as bs xs)
--     -- Subset as bs (x ': xs) = (Subset as bs xs)

-- -- | For each x in xs, create a (KnownNat (IndexOf (TypeAt i xs) ys)) contraint
-- type family Subset2 (smaller :: [Type]) (larger :: [Type]) :: Constraint where
--     Subset2 '[] ys = ()
--     Subset2 (x ': xs) ys = (Member x ys, Subset2 xs ys)

type family Length (xs :: [Type]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

-- -- | Check that a type is member of a type list
-- type IsMember x xs = IsMemberEx xs x xs

-- type family Typeables (xs :: [Type]) :: Constraint where
--     Typeables '[] = ()
--     Typeables (x ': xs) = (Typeable x, Typeables xs)

-- | A Read x For each x in xs
type family AllRead (xs :: [Type]) :: Constraint where
    AllRead '[] = ()
    AllRead (x ': xs) = (Read x, AllRead xs)

-- | A Read x For each x in xs
type family AllShow (xs :: [Type]) :: Constraint where
    AllShow '[] = ()
    AllShow (x ': xs) = (Show x, AllShow xs)

type family Tail (xs :: [Type]) :: [Type] where
    Tail '[] = TypeError ('Text "Cannot Head an empty type list")
    Tail (x ': xs) = xs

type family Head (xs :: [Type]) :: Type where
    Head '[] = TypeError ('Text "Cannot Head an empty type list")
    Head (x ': xs) = x

type SameLength (xs :: [Type]) (ys :: [Type]) = SameLengthImpl xs ys xs ys

-- | Set complement. Returns the set of things in xs that are not in ys.
type family Complement (xs :: [Type]) (ys :: [Type]) :: [Type] where
    Complement xs '[] = xs
    Complement xs (y ': ys)  = Complement (Without y xs) ys

-- -- | Check that a list is a subset of another
-- type family IsSubset smaller larger :: Bool where
--    IsSubset s l = IsSubsetEx l s l

-- -- | Check that a list is a subset of another
-- type family IsSubset smaller larger :: Bool where
--    IsSubset s l = IsSubsetEx l s l
