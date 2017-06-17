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

module Data.Diverse.Nary.Internal
    ( Nary(..) -- ^ Exporting constructor unsafely!
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
    , forNary
    , collect
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
import Data.Diverse.AFoldable
import Data.Diverse.Case
import Data.Diverse.Collector
import Data.Diverse.Emit
import Data.Diverse.Reiterate
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

-- | A Nary is an anonymous product type (also know as polymorphic record), that has fields of distinct types.
-- That is, there are no duplicates types in the fields of the record.
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to get and set fields in the Nary.
-- This encoding stores the fields as Any in a Map, where the key is index + offset of the type in the typelist.
-- The offset is used to allow efficient cons.
-- Key = Index of type in typelist + Offset
-- The constructor will guarantee the correct number and types of the elements.
newtype Key = Key Int deriving (Eq, Ord, Show)
newtype LeftOffset = LeftOffset Int
newtype LeftSize = LeftSize Int
newtype RightOffset = RightOffset Int
newtype NewRightOffset = NewRightOffset { unNewRightOffset :: Int }

data Nary (xs :: [Type]) = Nary {-# UNPACK #-} !Int (M.Map Key Any)

-- | Inferred role is phantom which is incorrect
type role Nary representational

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

blank :: Nary '[]
blank = Nary 0 M.empty
infixr 5 `blank` -- to be the same as cons

-- | 'blank' memonic. .| stops
-- Analogous to 'M.null'
(.|) :: Nary '[]
(.|) = blank

-- | Create a Nary from a single value. Analogous to 'M.singleton'
singleton :: x -> Nary '[x]
singleton v = Nary 0 (M.singleton (Key 0) (unsafeCoerce v))

-- | Add an element to the left of a Nary.
-- Not named 'cons' to avoid conflict with lens
prefix :: x -> Nary xs -> Nary (x ': xs)
prefix x (Nary ro rm) = Nary (unNewRightOffset nro)
    (M.insert
        (leftKeyForCons (LeftOffset 0) nro (Key 0))
        (unsafeCoerce x)
        rm)
  where
    nro = rightOffsetForCons (LeftSize 1) (RightOffset ro)
infixr 5 `prefix`

-- | 'cons' mnemonic. Element is smaller than ./ larger Nary
(./) :: x -> Nary xs -> Nary (x ': xs)
(./) = prefix
infixr 5 ./ -- like Data.List.(:)

-- | Add an element to the right of a Nary
-- Not named 'snoc' to avoid conflict with lens
postfix :: Nary xs -> y -> Nary (Concat xs '[y])
postfix (Nary lo lm) y = Nary lo
    (M.insert (rightKeyForSnoc (LeftOffset lo) (LeftSize (M.size lm)) (RightOffset 0) (Key 0))
        (unsafeCoerce y)
        lm)
infixl 5 `postfix`

-- | 'snoc' mnemonic. Nary is larger \. than smaller element
(\.) :: Nary xs -> y -> Nary (Concat xs '[y])
(\.) = postfix
infixl 5 \.

(/./) :: Nary xs -> Nary ys -> Nary (Concat xs ys)
(/./) = append
infixl 5 /./

-- | Contains two Narys together
append :: Nary xs -> Nary ys -> Nary (Concat xs ys)
append (Nary lo lm) (Nary ro rm) = if ld >= rd
    then Nary
         lo
         (lm `M.union` (M.mapKeys (rightKeyForSnoc (LeftOffset lo) (LeftSize ld) (RightOffset ro)) rm))
    else Nary
         (unNewRightOffset nro)
         ((M.mapKeys (leftKeyForCons (LeftOffset lo) nro) lm) `M.union` rm)
  where
    ld = M.size lm
    rd = M.size rm
    nro = rightOffsetForCons (LeftSize ld) (RightOffset ro)
infixr 5 `append` -- like Data.List (++)

-- | Extract the first element of a Nary, which guaranteed to be non-empty.
-- Analogous to 'Partial.head'
front :: Nary (x ': xs) -> x
front (Nary o m) = unsafeCoerce (m M.! (Key o))

-- | Extract the 'back' element of a Nary, which guaranteed to be non-empty.
-- Analogous to 'Prelude.last'
back :: Nary (x ': xs) -> x
back (Nary o m) = unsafeCoerce (m M.! (Key (M.size m + o)))

-- | Extract the elements after the front of a Nary, which guaranteed to be non-empty.
-- Analogous to 'Partial.tail'
aft :: Nary (x ': xs) -> Nary xs
aft (Nary o m) = Nary (o + 1) (M.delete (Key o) m)

-- | Return all the elements of a Nary except the 'back' one, which guaranteed to be non-empty.
-- Analogous to 'Prelude.init'
fore :: Nary xs -> Nary (Init xs)
fore (Nary o m) = Nary o (M.delete (Key (o + M.size m - 1)) m)

-- | Getter. Use TypeApplication of the type to get
-- Only available for 'Nary' with 'Distinct' xs.
fetch :: forall x xs. (Distinct xs, Member x xs) => Nary xs -> x
fetch (Nary o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix 'fetch' mnemonic. Like 'Control.Lens.(^.)' but with an extra dot in front.
(.^.) :: forall x xs. (Distinct xs, Member x xs) => Nary xs -> x
(.^.) = fetch
infixl 8 .^. -- like Control.Lens.(^.)

-- | Setter. Use TypeApplication of the type to set.
-- Only available for 'Nary' with 'Distinct' xs.
replace :: forall x xs. (Distinct xs, Member x xs) => Nary xs -> x -> Nary xs
replace (Nary o m) v = Nary o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix 'replace' mnemonic. Like 'Control.Lens.(.~)' but with an extra dot in front.
(..~) :: forall x xs. (Distinct xs, Member x xs) => Nary xs -> x -> Nary xs
(..~) = replace
infixl 1 ..~ -- like Control.Lens.(.~)

-- | Use TypeApplication to specify the field type of the lens.
-- Example: @item \@Int@
item :: forall x xs. (Distinct xs, Member x xs) => Lens' (Nary xs) x
item = lens fetch replace
{-# INLINE item #-}

-----------------------------------------------------------------------

-- | Wraps a 'Case' into an instance of 'Emit', feeding 'Case' with the value from the Nary and 'emit'ting the results.
-- Internally, this holds the left-over [(k, v)] from the original Nary with the remaining typelist xs.
-- That is the first v in the (k, v) is of type x, and the length of the list is equal to the length of xs.
newtype Via c (xs :: [Type]) r = Via (c xs r, [Any])

-- | Creates an 'Via' safely, so that the invariant of \"typelist to the value list type and size\" holds.
via :: forall xs c r. c xs r -> Nary xs -> Via c xs r
via c (Nary _ m) = Via (c, snd <$> M.toAscList m)

instance Reiterate c (x ': xs) => Reiterate (Via c) (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (Via (c, xxs)) = Via (reiterate c, Partial.tail xxs)

instance (Case c (x ': xs) r) => Emit (Via c) (x ': xs) r where
    emit (Via (c, xxs)) = then' c (unsafeCoerce v)
      where
       -- use of front here is safe as we are guaranteed the length from the typelist
       v = Partial.head xxs

forNary :: c xs r -> Nary xs -> Collector (Via c) xs r
forNary c x = Collector (via c x)

collect :: Nary xs -> c xs r -> Collector (Via c) xs r
collect = flip forNary

-----------------------------------------------------------------------

-- | Internal function for construction - do not expose!
fromList' :: Ord k => [(k, WrappedAny)] -> M.Map k Any
fromList' xs = M.fromList (coerce xs)

-----------------------------------------------------------------------

type Narrow smaller larger = (AFoldable (Collector (Via (CaseNarrow smaller)) larger) [(Key, WrappedAny)], Distinct larger, Distinct smaller)

narrow :: forall smaller larger. Narrow smaller larger => Nary larger -> Nary smaller
narrow xs = Nary 0 (fromList' xs')
  where
    xs' = afoldr (++) [] (forNary (CaseNarrow @smaller @larger) xs)

-- | infix 'narrow' mnemonic. Like 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
(\^.) :: forall smaller larger. Narrow smaller larger => Nary larger -> Nary smaller
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

amend :: forall smaller larger. Amend smaller larger => Nary larger -> Nary smaller -> Nary larger
amend (Nary ro rm) xs = Nary ro (fromList' xs' `M.union` rm)
  where
    xs' = afoldr (:) [] (forNary (CaseAmend @smaller @larger @smaller ro) xs)

-- | infix 'flip amend' mnemonic. Like 'Control.Lens.(.~)' but with an extra '\' (narrow to the right) in front.
(\.~) :: forall smaller larger. Amend smaller larger => Nary larger -> Nary smaller -> Nary larger
(\.~) = amend
infixl 1 \.~ -- like Control.Lens.(.~)

newtype CaseAmend (smaller :: [Type]) (larger :: [Type]) (xs :: [Type]) r = CaseAmend Int

instance Reiterate (CaseAmend smaller larger) (x ': xs) where
    reiterate (CaseAmend ro) = CaseAmend ro

-- | for each x in @Nary smaller@, convert it to a (k, v) to insert into the x in @Nary larger@
instance Member x larger => Case (CaseAmend smaller larger) (x ': xs) (Key, WrappedAny) where
    then' (CaseAmend ro) v = (Key (ro + i), WrappedAny (unsafeCoerce v))
      where
        i = fromInteger (natVal @(IndexOf x larger) Proxy)

-----------------------------------------------------------------------

-- | Projection.
-- A Nary can be narrowed or have its order changed by projecting into another Nary type.
-- Use TypeApplication to specify the @smaller@ typelist of the lens.
-- Example: @project \@'[Int, String]@
project :: forall smaller larger. (Narrow smaller larger, Amend smaller larger) => Lens' (Nary larger) (Nary smaller)
project = lens narrow amend
{-# INLINE project #-}

-- | This is 'project' with the type parameters reversed
-- so TypeApplications can be used to specify @larger@ typelist nstead of @smaller@.
-- Example: @projected \@'[Int, String]@
projected :: forall larger smaller. (Narrow smaller larger, Amend smaller larger) => Lens' (Nary larger) (Nary smaller)
projected = project
{-# INLINE projected #-}

-----------------------------------------------------------------------

-- | Stores the left & right Nary and a list of Any which must be the same length and types in xs typelist.
newtype EmitEqNary (xs :: [Type]) r = EmitEqNary ([Any], [Any])

instance Reiterate EmitEqNary (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitEqNary (ls, rs)) = EmitEqNary (Partial.tail ls, Partial.tail rs)

instance Eq x => Emit EmitEqNary (x ': xs) Bool where
    emit (EmitEqNary (ls, rs)) = l == r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

eqNary
    :: forall xs.
       AFoldable (Collector EmitEqNary xs) Bool
    => Nary xs -> Nary xs -> [Bool]
eqNary (Nary _ lm) (Nary _ rm) = afoldr (:) [] (Collector (EmitEqNary @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance AFoldable (Collector EmitEqNary xs) Bool => Eq (Nary xs) where
    lt == rt = foldr (\e z -> bool False z e) True eqs
      where
        eqs = eqNary lt rt

-----------------------------------------------------------------------

-- | Stores the left & right Nary and a list of Any which must be the same length and types in xs typelist.
newtype EmitOrdNary (xs :: [Type]) r = EmitOrdNary ([Any], [Any])

instance Reiterate EmitOrdNary (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitOrdNary (ls, rs)) = EmitOrdNary (Partial.tail ls, Partial.tail rs)

instance Ord x => Emit EmitOrdNary (x ': xs) Ordering where
    emit (EmitOrdNary (ls, rs)) = compare l r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

ordNary
    :: forall xs.
       AFoldable (Collector EmitOrdNary xs) Ordering
    => Nary xs -> Nary xs -> [Ordering]
ordNary (Nary _ lm) (Nary _ rm) = afoldr (:) [] (Collector (EmitOrdNary @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance (Eq (Nary xs), AFoldable (Collector EmitOrdNary xs) Ordering) => Ord (Nary xs) where
    compare lt rt = foldr (\o z -> case o of
                                       EQ -> z
                                       o' -> o') EQ ords
      where
        ords = ordNary lt rt

-----------------------------------------------------------------------

-- | Internally uses [Any] like Via, except also handle the empty type list.
newtype EmitShowNary (xs :: [Type]) r = EmitShowNary [Any]

instance Reiterate EmitShowNary (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitShowNary xxs) = EmitShowNary (Partial.tail xxs)

instance Emit EmitShowNary '[] ShowS where
    emit _ = showString ".|"

instance Show x => Emit EmitShowNary '[x] ShowS where
    emit (EmitShowNary xs) = showsPrec (cons_prec + 1) v
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xs) :: x
        cons_prec = 5 -- infixr 5 cons

instance Show x => Emit EmitShowNary (x ': x' ': xs) ShowS where
    emit (EmitShowNary xxs) = showsPrec (cons_prec + 1) v . showString "./"
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xxs) :: x
        cons_prec = 5 -- infixr 5 cons

showNary
    :: forall xs.
       AFoldable (Collector EmitShowNary xs) ShowS
    => Nary xs -> ShowS
showNary (Nary _ m) = afoldr (.) id (Collector (EmitShowNary @xs (snd <$> M.toAscList m)))

instance AFoldable (Collector EmitShowNary xs) ShowS => Show (Nary xs) where
    showsPrec d t = showParen (d > cons_prec) $ showNary t
      where
        cons_prec = 5 -- infixr 5 cons

-----------------------------------------------------------------------

newtype EmitReadNary (xs :: [Type]) r = EmitReadNary Key

instance Reiterate EmitReadNary (x ': xs) where
    reiterate (EmitReadNary (Key i)) = EmitReadNary (Key (i + 1))

instance Emit EmitReadNary '[] (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadNary _) = do
        lift $ L.expect (Symbol ".|")
        pure []

instance Read x => Emit EmitReadNary '[x] (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadNary i) = do
        a <- readPrec @x
        -- don't read the ".|", save that for Emit '[]
        pure [(i, WrappedAny (unsafeCoerce a))]

instance Read x => Emit EmitReadNary (x ': x' ': xs) (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadNary i) = do
        a <- readPrec @x
        lift $ L.expect (Symbol "./")
        pure [(i, WrappedAny (unsafeCoerce a))]

readNary
    :: forall xs.
       AFoldable (Collector0 EmitReadNary xs) (ReadPrec [(Key, WrappedAny)])
    => Proxy (xs :: [Type]) -> ReadPrec [(Key, WrappedAny)]
readNary _ = afoldr (liftA2 (++)) (pure []) (Collector0 (EmitReadNary @xs (Key 0)))

instance ( Distinct xs
         , AFoldable (Collector0 EmitReadNary xs) (ReadPrec [(Key, WrappedAny)])
         ) =>
         Read (Nary xs) where
    readPrec =
        parens $
        prec 10 $ do
            xs <- readNary @xs Proxy
            pure (Nary 0 (fromList' xs))

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any

-- FIXME: Add tuple conversion functions?
