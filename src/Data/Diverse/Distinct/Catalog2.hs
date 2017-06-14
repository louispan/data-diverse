{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Diverse.Distinct.Catalog2 where

import Data.Diverse.TypeLevel
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup hiding (Any)
import GHC.Prim (Any)
import GHC.TypeLits
import Unsafe.Coerce

-- TODO: Implement Data.Indistinct.Tuplex with getByIndex only semantics
-- TODO: Implement Data.Indistinct.Variant with getByIndex only semantics
-- TODO: Implement Data.Labelled.Record with getByLabel only semantics, where fields may only contain tagged values called :=
-- TODO: Implement Data.Labelled.Corecord with getByLabel only semantics, where fields may only contain tagged values called :=

-- | A Catalog is an anonymous product type (also know as polymorphic record), that has fields of distinct types.
-- That is, there are no duplicates types in the fields of the record.
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to get and set fields in the Catalog.
-- This encoding stores the fields as Any in a Map, where the key is index + offset of the type in the typelist.
-- The offset is used to allow efficient cons.
-- Key = Index of type in typelist + Offset
-- The constructor will guarantee the correct number and types of the elements.
newtype Key = Key Int deriving (Eq, Ord, Show)
newtype LeftOffset = LeftOffset Int
newtype LeftSize = LeftSize Int
newtype RightOffset = RightOffset Int
newtype NewRightOffset = NewRightOffset { unNewRightOffset :: Int }

data Catalog (xs :: [Type]) = Catalog {-# UNPACK #-} !Int (M.Map Key Any)

-- | Inferred role is phantom which is incorrect
type role Catalog representational

-- | When appending two maps together, get the function to 'M.mapKeys' the RightMap
-- when adding RightMap into LeftMap.
-- The existing contents of LeftMap will not be changed.
-- LeftMap Offset will also not change.
-- The desired key for element from the RightMap = RightIndex (of the element) + LeftOffset + LeftSize
-- OldRightKey = RightIndex + RightOffset, therefore RightIndex = OldRightKey - RightOffset
-- So we need to adjust the existing index on the RightMap by
-- \OldRightKey -> RightIndex + LeftOffset + LeftSize (as above)
-- \OldRightKey -> OldRightKey - RightOffset + LeftOffset + LeftSize
rightKeyForSnoc :: LeftOffset -> LeftSize -> RightOffset -> Key -> Key
rightKeyForSnoc (LeftOffset lo) (LeftSize ld) (RightOffset ro) (Key rk) =
    Key (rk - ro + lo + ld)

-- | When appending two maps together, get the function to modify the RightMap's offset
-- when adding LeftMap into RightMap.
-- The existing contents of RightMap will not be changed.
-- NewRightOffset = OldRightOffset - LeftSize
rightOffsetForCons :: LeftSize -> RightOffset -> NewRightOffset
rightOffsetForCons (LeftSize ld) (RightOffset ro) = NewRightOffset (ro - ld)

-- | When appending two maps together, get the function to 'M.mapKeys' the LeftMap
-- when adding LeftMap into RightMap.
-- The existing contents of RightMap will not be changed.
-- The RightMap's offset will be adjusted using 'rightOffsetWithRightMapUnchanged'
-- The desired key for the elements in the the LeftMap = LeftIndex (of the element) + NewRightOffset
-- OldLeftKey = LeftIndex + LeftOffset, therefore LeftIndex = OldLeftKey - LeftOffset
-- So we need to adjust the existing index on the LeftMap by
-- \OldLeftKey -> LeftIndex + NewRightOffset (as above)
-- \OldLeftKey -> OldLeftKey - LeftOffset + NewRightOffset (as above)
leftKeyForCons :: LeftOffset -> NewRightOffset -> Key -> Key
leftKeyForCons (LeftOffset lo) (NewRightOffset ro) (Key lk) = Key (lk - lo + ro)


null :: Catalog '[]
null = Catalog 0 M.empty

singleton :: x -> Catalog '[x]
singleton v = Catalog 0 (M.singleton (Key 0) (unsafeCoerce v))

-- | Add an element to the left of the typelist
cons :: Distinct (x ': xs) => x -> Catalog xs -> Catalog (x ': xs)
cons x (Catalog ro rm) = Catalog (unNewRightOffset nro)
    (M.insert
        (leftKeyForCons (LeftOffset 0) nro (Key 0))
        (unsafeCoerce x)
        rm)
  where
    nro = rightOffsetForCons (LeftSize 1) (RightOffset ro)

-- | Add an element to the right of the typelist
snoc :: Distinct (Concat xs '[y]) => Catalog xs -> y -> Catalog (Concat xs '[y])
snoc (Catalog lo lm) y = Catalog lo
    (M.insert (rightKeyForSnoc (LeftOffset lo) (LeftSize (M.size lm)) (RightOffset 0) (Key 0))
        (unsafeCoerce y)
        lm)

append :: Catalog xs -> Catalog ys -> Catalog (Concat xs ys)
append (Catalog lo lm) (Catalog ro rm) = if ld >= rd
    then Catalog
         lo
         (lm `M.union` (M.mapKeys (rightKeyForSnoc (LeftOffset lo) (LeftSize ld) (RightOffset ro)) rm))
    else Catalog
         (unNewRightOffset nro)
         ((M.mapKeys (leftKeyForCons (LeftOffset lo) nro) lm) `M.union` rm)
  where
    ld = M.size lm
    rd = M.size rm
    nro = rightOffsetForCons (LeftSize ld) (RightOffset ro)

-- | Extract the first element of a Catalog, which must be non-empty.
head :: Catalog (x ': xs) -> x
head (Catalog o m) = unsafeCoerce (m M.! (Key o))

-- | Extract the last element of a Catalog, which must be finite and non-empty.
last :: Catalog (x ': xs) -> x
last (Catalog o m) = unsafeCoerce (m M.! (Key (M.size m + o)))

-- | Extract the elements after the head of a Catalog, which must be non-empty.
tail :: Catalog (x ': xs) -> Catalog xs
tail (Catalog o m) = Catalog (o + 1) (M.delete (Key o) m)

-- | Return all the elements of a Catalog except the last one. The Catalog must be non-empty.
init :: Catalog xs -> Catalog (Init xs)
init (Catalog o m) = Catalog o (M.delete (Key (o + M.size m - 1)) m)

lookup :: forall x xs. Member x xs => Catalog xs -> x
lookup (Catalog o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromIntegral (natVal @(IndexOf x xs) Proxy)

replace :: forall x xs. Member x xs => x -> Catalog xs -> Catalog xs
replace v (Catalog o m) = Catalog o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromIntegral (natVal @(IndexOf x xs) Proxy)

internalFromList :: Distinct xs => [(Key, Any)] -> Catalog xs
internalFromList xs = Catalog 0 (M.fromList xs)

narrow :: forall ys xs. (MembersOf xs xs, MembersOf ys xs) => Catalog xs -> Catalog ys
narrow = undefined
-- for each y in ys
-- produce a (Key, Any)

class Generate g (xs :: [Type]) r where
    generate :: g xs r -> r

class Step g (xs :: [Type]) r where
    step :: g xs r -> r

class Next g (xs :: [Type]) r where
    next :: g xs r -> g (Tail xs) r

instance (Step g '[] r) => Generate g '[] r where
    generate g = step g

newtype Generator g (xs :: [Type]) r = Generator (g (xs :: [Type]) r)

instance (Semigroup r, Step g (x ': xs) r, Step g xs r, Next g (x ': xs) r, Generate (Generator g) xs r) =>
         Generate (Generator g) (x ': xs) r where
    generate (Generator g) = step g <> generate (Generator (next g))

-- | Avoids: Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any

newtype GenerateNarrowed (ys :: [Type]) (xs :: [Type]) r = GenerateNarrowed (Key, Catalog ys)

instance Step (GenerateNarrowed ys) '[] [(Key, WrappedAny)] where
    step _ = []

instance Member x ys => Step (GenerateNarrowed ys) (x ': xs) [(Key, WrappedAny)] where
    step (GenerateNarrowed (k, Catalog o m)) = [(k, WrappedAny (m M.! (Key (o + i))))]
      where i = fromIntegral (natVal @(IndexOf x ys) Proxy)

-- | Don't create an instance for (xs :: '[])
instance Next (GenerateNarrowed ys) (x ': xs) r where
    next (GenerateNarrowed (Key k, c)) = GenerateNarrowed (Key (k + 1), c)
