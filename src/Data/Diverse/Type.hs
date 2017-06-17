{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Type where

import Data.Diverse.Type.Internal
import Data.Kind
import GHC.TypeLits

--FIXME: remove unused after Catalog is deprecated

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
    TypesOf (a, b, c, d) = '[a, b, c, d]
    TypesOf (a, b, c, d, e) = '[a, b, c, d, e]
    TypesOf (a, b, c, d, e, f) = '[a, b, c, d, e, f]
    TypesOf (a, b, c, d, e, f, g) = '[a, b, c, d, e, f, g]
    TypesOf (a, b, c, d, e, f, g, h) = '[a, b, c, d, e, f, g, h]
    TypesOf (a, b, c, d, e, f, g, h, i) = TypesOfNotSupported (a, b, c, d, e, f, g, h, i)
    -- TypesOf (a, b, c, d) = TypesOfNotSupported (a, b, c, d)
    -- declare overlapping instance last in this closed type family
    TypesOf a = '[a]

-- | Get the tuple with equivalent type list
-- The size of this should be synchronized withe the number of 'Has' instances in Catalog
type family TupleOf (xs :: [Type]) :: Type where
    TupleOf '[] = ()
    TupleOf '[a, b] = (a, b)
    TupleOf '[a, b, c] = (a, b, c)
    TupleOf '[a, b, c, d] = (a, b, c, d)
    TupleOf '[a, b, c, d, e] = (a, b, c, d, e)
    TupleOf '[a, b, c, d, e, f] = (a, b, c, d, e, f)
    TupleOf '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
    TupleOf '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
    TupleOf (a ': b ': c ': d ': e ': f ': g ': h ': i ': xs) = TupleOfNotSupported (a ': b ': c ': d ': e ': f ': g ': h ': i ': xs)
    -- TupleOf (a ': b ': c ': d ': xs) = TupleOfNotSupported (a ': b ': c ': d ': xs)
    -- declare overlapping instance last in this closed type family
    TupleOf '[a] = a

-- | Add a type to a typelist, disallowing duplicates.
-- NB. xs are not checked.
type Insert (y :: Type) (xs :: [Type]) = InsertImpl xs y xs

-- | Combine two type lists together, assuming disallowing duplicates from ys
-- NB. xs are not checked.
type family Union (xs :: [Type]) (ys :: [Type]) :: [Type] where
    Union '[] '[] = '[]
    Union xs '[] = xs
    Union xs (y ': ys) = Union (Insert y xs) ys

-- | A constraint ensuring that the type list contain unique types
type Distinct (xs :: [Type]) = Union '[] xs ~ xs

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
type IndexOf x (xs :: [Type]) = IndexOfImpl xs x xs

-- | Get the first index of a type (Indexed by 1)
-- Will return 0 if x doesn't exists in xs.
type PositionOf x (xs :: [Type]) = PositionOfImpl 0 x xs

-- | Get the type at an index
type TypeAt (n :: Nat) (xs :: [Type]) = TypeAtImpl n xs n xs

-- | The typelist xs without x. It is okay for x not to exist in xs
type Without x (xs :: [Type]) = WithoutImpl x '[] xs

type Reverse (xs :: [Type]) = ReverseImpl '[] xs

-- | KnownNat constraint is proof to GHC that it can instantiate a value of KnownNat
-- for a particular typelevel Nat for a particular usage of 'natVal'.
-- 'Member' is required for using 'natVal' for a particular index (starting from 0)
-- of a type in a typelist.
type Member x xs = (KnownNat (IndexOf x xs))

-- | Index is within Bounds of the typelist
type WithinBounds (n :: Nat) (xs :: [Type]) = (KnownNat n, n + 1 <= Length xs, 0 <= n)

-- | KnownNat constraint is proof to GHC that it can instantiate a value of KnownNat
-- for a particular typelevel Nat for a particular usage of 'natVal'.
-- MaybeMember required to use 'natVal' for a particular position (starting from 1)
-- of a type in a typelist, or 0 if the type is not found.
type MaybeMember x xs = KnownNat (PositionOf x xs)

-- | For all x in xs, provide a proof that there is a KnownNat of x in ys.
type family MembersOf (ctx :: [Type]) (xs :: [Type]) :: Constraint where
    MembersOf ctx '[] = ()
    MembersOf ctx (x ': xs) = (Member x ctx,  MembersOf ctx xs)

type family Length (xs :: [Type]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Tail (xs :: [Type]) :: [Type] where
    Tail '[] = TypeError ('Text "Cannot Tail an empty type list")
    Tail (x ': xs) = xs

type family Head (xs :: [Type]) :: Type where
    Head '[] = TypeError ('Text "Cannot Head an empty type list")
    Head (x ': xs) = x

type SameLength (xs :: [Type]) (ys :: [Type]) = SameLengthImpl xs ys xs ys

-- | Set complement. Returns the set of things in xs that are not in ys.
type family Complement (xs :: [Type]) (ys :: [Type]) :: [Type] where
    Complement xs '[] = xs
    Complement xs (y ': ys)  = Complement (Without y xs) ys

type family Concat (xs :: [Type]) (ys :: [Type]) :: [Type] where
    Concat '[] ys = ys
    Concat (x ': xs) ys = x ': Concat xs ys

type Init (xs :: [Type]) = InitImpl '[] xs

-- | The follows breaks the constraint sovler
-- solveWanteds: too many iterations (limit = 4)
--   Unsolved: WC {wc_simple = [W] $dShow_acLn :: Show g (CDictCan)}
--   New superclasses found
--   Set limit with -fconstraint-solver-iterations=n; n=0 for no limit

-- -- | @Read x@ For each x in xs
-- type family AllRead (xs :: [Type]) :: Constraint where
--     AllRead '[] = ()
--     AllRead (x ': xs) = (Read x, AllRead xs)

-- -- | @Show x@ For each x in xs
-- type family AllShow (xs :: [Type]) :: Constraint where
--     AllShow '[] = ()
--     AllShow (x ': xs) = (Show x, AllShow xs)

-- -- | @Eq x@ For each x in xs
-- type family AllEq (xs :: [Type]) :: Constraint where
--     AllEq '[] = ()
--     AllEq (x ': xs) = (Eq x, AllEq xs)

-- -- | @Eq x@ For each x in xs
-- type family AllOrd (xs :: [Type]) :: Constraint where
--     AllOrd '[] = ()
--     AllOrd (x ': xs) = (Ord x, AllOrd xs)
