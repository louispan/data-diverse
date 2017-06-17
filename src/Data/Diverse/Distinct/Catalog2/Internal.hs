{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Diverse.Distinct.Catalog2.Internal
    ( Catalog(..) -- ^ Exporting constructor unsafely!
    , blank
    , (.|)
    , singleton
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)
    , front
    , back
    , aft
    , fore
    , fetch
    , (.^.)
    , replace
    , (..~)
    , item
    , Via -- ^ no constructor
    , via -- ^ safe construction
    , forCatalog
    , collect
    , Cases(..)
    , Narrow
    , narrow
    , (\^.)
    , Amend
    , amend
    , (\.~)
    , project
    , projected
    ) where

import Control.Applicative
import Control.Lens
import Data.Bool
import Data.Diverse.Class.AFoldable
import Data.Diverse.Class.Case
import Data.Diverse.Class.Emit
import Data.Diverse.Class.Reiterate
import Data.Diverse.Data.Collector
import Data.Diverse.Data.WrappedAny
import Data.Diverse.Type
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import GHC.Prim (Any, coerce)
import GHC.TypeLits
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L
import Unsafe.Coerce

-- | This module uses the partial 'head', 'tail' from Prelude.
-- I like to highlight them as partial by using them in the namespace Partial.head
-- These usages in this are safe due to size guarantees provided by the typelist
-- as long as the initial of the list matches the typelist.
import Prelude as Partial

-- TODO: Implement Data.Indistinct.Tuplex or NAry with getByIndex only semantics
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

blank :: Catalog '[]
blank = Catalog 0 M.empty
infixr 5 `blank` -- to be the same as cons

-- | 'blank' memonic. .| stops
-- Analogous to 'M.null'
(.|) :: Catalog '[]
(.|) = blank

-- | Create a Catalog from a single value. Analogous to 'M.singleton'
singleton :: x -> Catalog '[x]
singleton v = Catalog 0 (M.singleton (Key 0) (unsafeCoerce v))

-- | Add an element to the left of a Catalog.
-- Not named 'cons' to avoid conflict with lens
prefix :: x -> Catalog xs -> Catalog (x ': xs)
prefix x (Catalog ro rm) = Catalog (unNewRightOffset nro)
    (M.insert
        (leftKeyForCons (LeftOffset 0) nro (Key 0))
        (unsafeCoerce x)
        rm)
  where
    nro = rightOffsetForCons (LeftSize 1) (RightOffset ro)
infixr 5 `prefix`

-- | 'cons' mnemonic. Element is smaller than ./ larger Catalog
(./) :: x -> Catalog xs -> Catalog (x ': xs)
(./) = prefix
infixr 5 ./ -- like Data.List.(:)

-- | Add an element to the right of a Catalog
-- Not named 'snoc' to avoid conflict with lens
postfix :: Catalog xs -> y -> Catalog (Concat xs '[y])
postfix (Catalog lo lm) y = Catalog lo
    (M.insert (rightKeyForSnoc (LeftOffset lo) (LeftSize (M.size lm)) (RightOffset 0) (Key 0))
        (unsafeCoerce y)
        lm)
infixl 5 `postfix`

-- | 'snoc' mnemonic. Catalog is larger \. than smaller element
(\.) :: Catalog xs -> y -> Catalog (Concat xs '[y])
(\.) = postfix
infixl 5 \.

(/./) :: Catalog xs -> Catalog ys -> Catalog (Concat xs ys)
(/./) = append
infixl 5 /./

-- | Contains two Catalogs together
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
infixr 5 `append` -- like Data.List (++)

-- | Extract the first element of a Catalog, which guaranteed to be non-empty.
-- Analogous to 'Partial.head'
front :: Catalog (x ': xs) -> x
front (Catalog o m) = unsafeCoerce (m M.! (Key o))

-- | Extract the 'back' element of a Catalog, which guaranteed to be non-empty.
-- Analogous to 'Prelude.last'
back :: Catalog (x ': xs) -> x
back (Catalog o m) = unsafeCoerce (m M.! (Key (M.size m + o)))

-- | Extract the elements after the front of a Catalog, which guaranteed to be non-empty.
-- Analogous to 'Partial.tail'
aft :: Catalog (x ': xs) -> Catalog xs
aft (Catalog o m) = Catalog (o + 1) (M.delete (Key o) m)

-- | Return all the elements of a Catalog except the 'back' one, which guaranteed to be non-empty.
-- Analogous to 'Prelude.init'
fore :: Catalog xs -> Catalog (Init xs)
fore (Catalog o m) = Catalog o (M.delete (Key (o + M.size m - 1)) m)

-- | Getter. Use TypeApplication of the type to get
-- Only available for 'Catalog' with 'Distinct' xs.
fetch :: forall x xs. (Distinct xs, Member x xs) => Catalog xs -> x
fetch (Catalog o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix 'fetch' mnemonic. Like 'Control.Lens.(^.)' but with an extra dot in front.
(.^.) :: forall x xs. (Distinct xs, Member x xs) => Catalog xs -> x
(.^.) = fetch
infixl 8 .^. -- like Control.Lens.(^.)

-- | Setter. Use TypeApplication of the type to set.
-- Only available for 'Catalog' with 'Distinct' xs.
replace :: forall x xs. (Distinct xs, Member x xs) => Catalog xs -> x -> Catalog xs
replace (Catalog o m) v = Catalog o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix 'replace' mnemonic. Like 'Control.Lens.(.~)' but with an extra dot in front.
(..~) :: forall x xs. (Distinct xs, Member x xs) => Catalog xs -> x -> Catalog xs
(..~) = replace
infixl 1 ..~ -- like Control.Lens.(.~)

-- | Use TypeApplication to specify the field type of the lens.
-- Example: @item \@Int@
item :: forall x xs. (Distinct xs, Member x xs) => Lens' (Catalog xs) x
item = lens fetch replace
{-# INLINE item #-}

-----------------------------------------------------------------------

-- | Wraps a 'Case' into an instance of 'Emit', feeding 'Case' with the value from the Catalog and 'emit'ting the results.
-- Internally, this holds the left-over [(k, v)] from the original Catalog with the remaining typelist xs.
-- That is the first v in the (k, v) is of type x, and the length of the list is equal to the length of xs.
newtype Via c (xs :: [Type]) r = Via (c xs r, [Any])

-- | Creates an 'Via' safely, so that the invariant of \"typelist to the value list type and size\" holds.
via :: forall xs c r. c xs r -> Catalog xs -> Via c xs r
via c (Catalog _ m) = Via (c, snd <$> M.toAscList m)

instance Reiterate c (x ': xs) => Reiterate (Via c) (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (Via (c, xxs)) = Via (reiterate c, Partial.tail xxs)

instance (Case c (x ': xs) r) => Emit (Via c) (x ': xs) r where
    emit (Via (c, xxs)) = then' c (unsafeCoerce v)
      where
       -- use of front here is safe as we are guaranteed the length from the typelist
       v = Partial.head xxs

forCatalog :: c xs r -> Catalog xs -> Collector (Via c) xs r
forCatalog c x = Collector (via c x)

collect :: Catalog xs -> c xs r -> Collector (Via c) xs r
collect = flip forCatalog

-----------------------------------------------------------------------

-- | Contains a 'Catalog' of handlers/continuations for all the types in the 'xs' typelist.
newtype Cases (fs :: [Type]) (xs :: [Type]) r = Cases (Catalog fs)

instance Reiterate (Cases fs) xs where
    reiterate (Cases s) = Cases s

instance (Distinct fs, Member (Head xs -> r) fs) => Case (Cases fs) xs r where
    then' (Cases s) = fetch @(Head xs -> r) s

-- | Internal function for construction - do not expose!
fromList' :: Ord k => [(k, WrappedAny)] -> M.Map k Any
fromList' xs = M.fromList (coerce xs)

-----------------------------------------------------------------------

type Narrow smaller larger = (AFoldable (Collector (Via (CaseNarrow smaller)) larger) [(Key, WrappedAny)], Distinct larger, Distinct smaller)

narrow :: forall smaller larger. Narrow smaller larger => Catalog larger -> Catalog smaller
narrow xs = Catalog 0 (fromList' xs')
  where
    xs' = afoldr (++) [] (forCatalog (CaseNarrow @smaller @larger) xs)

-- | infix 'narrow' mnemonic. Like 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
(\^.) :: forall smaller larger. Narrow smaller larger => Catalog larger -> Catalog smaller
(\^.) = narrow
infixl 8 \^. -- like Control.Lens.(^.)

-- | For each type x in @larger@, generate the (k, v) in @smaller@ (if it exists)
-- This stores the larger catalog map in a list form.
-- Like 'Via', the list is guaranteed to be the same size and type as @xs@.
-- xs is originally the same as larger
data CaseNarrow (smaller :: [Type]) (xs :: [Type]) r = CaseNarrow

instance Reiterate (CaseNarrow smaller) (x ': xs) where
    reiterate CaseNarrow = CaseNarrow

-- | For each type x in larger, find the index in ys, and create an (incrementing key, value)
-- instance forall smaller x xs. MaybeMember x smaller => Emit (EmitNarrowed smaller) (x ': xs) [(Key, WrappedAny)] where
instance forall smaller x xs. MaybeMember x smaller => Case (CaseNarrow smaller) (x ': xs) [(Key, WrappedAny)] where
    then' _ v = case i of
                    0 -> []
                    i' -> [(Key (i' - 1), WrappedAny (unsafeCoerce v))]
      where
        i = fromInteger (natVal @(PositionOf x smaller) Proxy)

-----------------------------------------------------------------------
type Amend smaller larger = (AFoldable (Collector (Via (CaseAmend smaller larger)) smaller) (Key, WrappedAny)
       , Distinct larger
       , Distinct smaller)

amend :: forall smaller larger. Amend smaller larger => Catalog larger -> Catalog smaller -> Catalog larger
amend (Catalog ro rm) xs = Catalog ro (fromList' xs' `M.union` rm)
  where
    xs' = afoldr (:) [] (forCatalog (CaseAmend @smaller @larger @smaller ro) xs)

-- | infix 'flip amend' mnemonic. Like 'Control.Lens.(.~)' but with an extra '\' (narrow to the right) in front.
(\.~) :: forall smaller larger. Amend smaller larger => Catalog larger -> Catalog smaller -> Catalog larger
(\.~) = amend
infixl 1 \.~ -- like Control.Lens.(.~)

newtype CaseAmend (smaller :: [Type]) (larger :: [Type]) (xs :: [Type]) r = CaseAmend Int

instance Reiterate (CaseAmend smaller larger) (x ': xs) where
    reiterate (CaseAmend ro) = CaseAmend ro

-- | for each x in @Catalog smaller@, convert it to a (k, v) to insert into the x in @Catalog larger@
instance Member x larger => Case (CaseAmend smaller larger) (x ': xs) (Key, WrappedAny) where
    then' (CaseAmend ro) v = (Key (ro + i), WrappedAny (unsafeCoerce v))
      where
        i = fromInteger (natVal @(IndexOf x larger) Proxy)

-----------------------------------------------------------------------

-- | Projection.
-- A Catalog can be narrowed or have its order changed by projecting into another Catalog type.
-- Use TypeApplication to specify the @smaller@ typelist of the lens.
-- Example: @project \@'[Int, String]@
project :: forall smaller larger. (Narrow smaller larger, Amend smaller larger) => Lens' (Catalog larger) (Catalog smaller)
project = lens narrow amend
{-# INLINE project #-}

-- | This is 'project' with the type parameters reversed
-- so TypeApplications can be used to specify @larger@ typelist nstead of @smaller@.
-- Example: @projected \@'[Int, String]@
projected :: forall larger smaller. (Narrow smaller larger, Amend smaller larger) => Lens' (Catalog larger) (Catalog smaller)
projected = project
{-# INLINE projected #-}

-----------------------------------------------------------------------

-- | Stores the left & right Catalog and a list of Any which must be the same length and types in xs typelist.
newtype EmitEqCatalog (xs :: [Type]) r = EmitEqCatalog ([Any], [Any])

instance Reiterate EmitEqCatalog (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitEqCatalog (ls, rs)) = EmitEqCatalog (Partial.tail ls, Partial.tail rs)

instance Eq x => Emit EmitEqCatalog (x ': xs) Bool where
    emit (EmitEqCatalog (ls, rs)) = l == r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

eqCatalog
    :: forall xs.
       AFoldable (Collector EmitEqCatalog xs) Bool
    => Catalog xs -> Catalog xs -> [Bool]
eqCatalog (Catalog _ lm) (Catalog _ rm) = afoldr (:) [] (Collector (EmitEqCatalog @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance AFoldable (Collector EmitEqCatalog xs) Bool => Eq (Catalog xs) where
    lt == rt = foldr (\e z -> bool False z e) True eqs
      where
        eqs = eqCatalog lt rt

-----------------------------------------------------------------------

-- | Stores the left & right Catalog and a list of Any which must be the same length and types in xs typelist.
newtype EmitOrdCatalog (xs :: [Type]) r = EmitOrdCatalog ([Any], [Any])

instance Reiterate EmitOrdCatalog (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitOrdCatalog (ls, rs)) = EmitOrdCatalog (Partial.tail ls, Partial.tail rs)

instance Ord x => Emit EmitOrdCatalog (x ': xs) Ordering where
    emit (EmitOrdCatalog (ls, rs)) = compare l r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

ordCatalog
    :: forall xs.
       AFoldable (Collector EmitOrdCatalog xs) Ordering
    => Catalog xs -> Catalog xs -> [Ordering]
ordCatalog (Catalog _ lm) (Catalog _ rm) = afoldr (:) [] (Collector (EmitOrdCatalog @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance (Eq (Catalog xs), AFoldable (Collector EmitOrdCatalog xs) Ordering) => Ord (Catalog xs) where
    compare lt rt = foldr (\o z -> case o of
                                       EQ -> z
                                       o' -> o') EQ ords
      where
        ords = ordCatalog lt rt

-----------------------------------------------------------------------

-- | Internally uses [Any] like Via, except also handle the empty type list.
newtype EmitShowCatalog (xs :: [Type]) r = EmitShowCatalog [Any]

instance Reiterate EmitShowCatalog (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitShowCatalog xxs) = EmitShowCatalog (Partial.tail xxs)

instance Emit EmitShowCatalog '[] ShowS where
    emit _ = showString ".|"

instance Show x => Emit EmitShowCatalog '[x] ShowS where
    emit (EmitShowCatalog xs) = showsPrec (cons_prec + 1) v
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xs) :: x
        cons_prec = 5 -- infixr 5 cons

instance Show x => Emit EmitShowCatalog (x ': x' ': xs) ShowS where
    emit (EmitShowCatalog xxs) = showsPrec (cons_prec + 1) v . showString "./"
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xxs) :: x
        cons_prec = 5 -- infixr 5 cons

showCatalog
    :: forall xs.
       AFoldable (Collector EmitShowCatalog xs) ShowS
    => Catalog xs -> ShowS
showCatalog (Catalog _ m) = afoldr (.) id (Collector (EmitShowCatalog @xs (snd <$> M.toAscList m)))

instance AFoldable (Collector EmitShowCatalog xs) ShowS => Show (Catalog xs) where
    showsPrec d t = showParen (d > cons_prec) $ showCatalog t
      where
        cons_prec = 5 -- infixr 5 cons

-----------------------------------------------------------------------

newtype EmitReadCatalog (xs :: [Type]) r = EmitReadCatalog Key

instance Reiterate EmitReadCatalog (x ': xs) where
    reiterate (EmitReadCatalog (Key i)) = EmitReadCatalog (Key (i + 1))

instance Emit EmitReadCatalog '[] (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadCatalog _) = do
        lift $ L.expect (Symbol ".|")
        pure []

instance Read x => Emit EmitReadCatalog '[x] (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadCatalog i) = do
        a <- readPrec @x
        -- don't read the ".|", save that for Emit '[]
        pure [(i, WrappedAny (unsafeCoerce a))]

instance Read x => Emit EmitReadCatalog (x ': x' ': xs) (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadCatalog i) = do
        a <- readPrec @x
        lift $ L.expect (Symbol "./")
        pure [(i, WrappedAny (unsafeCoerce a))]

readCatalog
    :: forall xs.
       AFoldable (Collector0 EmitReadCatalog xs) (ReadPrec [(Key, WrappedAny)])
    => Proxy (xs :: [Type]) -> ReadPrec [(Key, WrappedAny)]
readCatalog _ = afoldr (liftA2 (++)) (pure []) (Collector0 (EmitReadCatalog @xs (Key 0)))

instance ( Distinct xs
         , AFoldable (Collector0 EmitReadCatalog xs) (ReadPrec [(Key, WrappedAny)])
         ) =>
         Read (Catalog xs) where
    readPrec =
        parens $
        prec 10 $ do
            xs <- readCatalog @xs Proxy
            pure (Catalog 0 (fromList' xs))

-- FIXME: Add tuple conversion functions?
