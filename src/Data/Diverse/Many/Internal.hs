{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Many.Internal (
    -- * 'Many' type
      Many(..) -- Exporting constructor unsafely!

      -- * Isomorphism
    , IsMany(..)
    , fromMany'
    , toMany'
    , _Many
    , _Many'

      -- * Construction
    , nul
    , single
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)

    -- * Simple queries
    , front
    , back
    , aft
    , fore

    -- * Single field
    -- ** Getter for single field
    , fetch
    , (.^.)
    , fetchN
    -- ** Setter for single field
    , replace
    , (.~.)
    , replaceN
    -- ** Lens for a single field
    , item
    , itemN

    -- * Multiple fields
    -- ** Getter for multiple fields
    , Narrow
    , narrow
    , (\^.)
    , NarrowN
    , narrowN
    -- ** Setter for multiple fields
    , Amend
    , amend
    , (\~.)
    , AmendN
    , amendN
    -- ** Lens for multiple fields
    , project
    , projectN

    -- * Destruction
    -- ** By type
    , Via -- no constructor
    , via -- safe construction
    , forMany
    , collect
    -- * By Nat index offset
    , ViaN -- no constructor
    , viaN -- safe construction
    , forManyN
    , collectN
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
import Data.Tagged
import GHC.Prim (Any, coerce)
import GHC.TypeLits
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L
import Unsafe.Coerce

-- This module uses the partial 'head', 'tail' from Prelude.
-- I like to highlight them as partial by using them in the namespace Partial.head
-- These usages in this are safe due to size guarantees provided by the typelist
-- as long as the initial of the list matches the typelist.
import Prelude as Partial

newtype Key = Key Int deriving (Eq, Ord, Show)
newtype LeftOffset = LeftOffset Int
newtype LeftSize = LeftSize Int
newtype RightOffset = RightOffset Int
newtype NewRightOffset = NewRightOffset { unNewRightOffset :: Int }

-- | A Many is an anonymous product type (also know as polymorphic record), with the ability to contain
-- an arbitrary number of fields.
--
-- When it has fields of unique types, extra functions applied via TypeApplications
-- become available to be used:
--
-- * 'fetch' and 'replace' getter/setter functions
-- * 'narrow' and 'amend' getter/setter of multiple fields
--
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to get and set fields in the Many.
-- It is a compile error to use those functions for duplicate fields.
-- For duplicate fields, there are indexed version of the gettter/setter functions.
--
-- This encoding stores the fields as Any in a Map, where the key is index + offset of the type in the typelist.
-- The offset is used to allow efficient cons.
--
-- @Key = Index of type in typelist + Offset@
--
-- The constructor will guarantee the correct number and types of the elements.
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Int (M.Map Key Any)

-- | Inferred role is phantom which is incorrect
type role Many representational

-----------------------------------------------------------------------

-- | This instance allows converting to and from Many
-- There are instances for converting tuples of up to size 15.
class IsMany t xs a where
    toMany :: t xs a -> Many xs
    fromMany :: Many xs -> t xs a

-- | Converts from a value (eg a tuple) to a Many, via a Tagged wrapper
toMany' :: IsMany Tagged xs a => a -> Many xs
toMany' a = toMany (Tagged a)

-- | Converts from a Many to a value (eg a tuple), via a Tagged wrapper
fromMany' :: IsMany Tagged xs a => Many xs -> a
fromMany' = unTagged . fromMany

-- | @_Many = iso fromMany toMany@
_Many :: IsMany t xs a => Iso' (Many xs) (t xs a)
_Many = iso fromMany toMany

-- | @_Many' = iso fromMany' toMany'@
_Many' :: IsMany Tagged xs a => Iso' (Many xs) a
_Many' = iso fromMany' toMany'

-- These instances add about 7 seconds to the compile time!

instance IsMany Tagged '[] () where
    toMany _ = nul
    fromMany _ = Tagged ()

instance IsMany Tagged '[a] a where
    toMany (Tagged a) = single a
    fromMany r = Tagged (fetch @a r)

instance IsMany Tagged '[a,b] (a,b) where
    toMany (Tagged (a,b)) = a./b./nul
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r)

instance IsMany Tagged '[a,b,c] (a,b,c) where
    toMany (Tagged (a,b,c)) = a./b./c./nul
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r)

instance IsMany Tagged '[a,b,c,d] (a,b,c,d) where
    toMany (Tagged (a,b,c,d)) = a./b./c./d./nul
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r)

instance IsMany Tagged '[a,b,c,d,e] (a,b,c,d,e) where
    toMany (Tagged (a,b,c,d,e)) = a./b./c./d./e./nul
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r)

instance IsMany Tagged '[a,b,c,d,e,f] (a,b,c,d,e,f) where
    toMany (Tagged (a,b,c,d,e,f)) = a./b./c./d./e./f./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r)

instance IsMany Tagged '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
    toMany (Tagged (a,b,c,d,e,f,g)) = a./b./c./d./e./f./g./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
    toMany (Tagged (a,b,c,d,e,f,g,h)) = a./b./c./d./e./f./g./h./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i] (a,b,c,d,e,f,g,h,i) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i)) = a./b./c./d./e./f./g./h./i./ nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j] (a,b,c,d,e,f,g,h,i,j) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j)) = a./b./c./d./e./f./g./h./i./j./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k] (a,b,c,d,e,f,g,h,i,j,k) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k)) = a./b./c./d./e./f./g./h./i./j./k./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l] (a,b,c,d,e,f,g,h,i,j,k,l) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l)) = a./b./c./d./e./f./g./h./i./j./k./l./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m] (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m)) = a./b./c./d./e./f./g./h./i./j./k./l./m./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m,n] (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) = a./b./c./d./e./f./g./h./i./j./k./l./m./n./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r, fetchN (Proxy @13) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) = a./b./c./d./e./f./g./h./i./j./k./l./m./n./o./nul
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r, fetchN (Proxy @13) r, fetchN (Proxy @14) r)

-----------------------------------------------------------------------

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

-- | Analogous to 'Prelude.null'. Named 'nul' to avoid conflicting with 'Prelude.null'.
nul :: Many '[]
nul = Many 0 M.empty
infixr 5 `nul` -- to be the same as cons

-- | Create a Many from a single value. Analogous to 'M.singleton'
single :: x -> Many '[x]
single v = Many 0 (M.singleton (Key 0) (unsafeCoerce v))

-- | Add an element to the left of a Many.
-- Not named 'cons' to avoid conflict with lens.
prefix :: x -> Many xs -> Many (x ': xs)
prefix x (Many ro rm) = Many (unNewRightOffset nro)
    (M.insert
        (leftKeyForCons (LeftOffset 0) nro (Key 0))
        (unsafeCoerce x)
        rm)
  where
    nro = rightOffsetForCons (LeftSize 1) (RightOffset ro)
infixr 5 `prefix`

-- | 'prefix' mnemonic: Element is smaller than './' the larger Many
(./) :: x -> Many xs -> Many (x ': xs)
(./) = prefix
infixr 5 ./ -- like Data.List.(:)

-- | Add an element to the right of a Many
-- Not named 'snoc' to avoid conflict with lens
postfix :: Many xs -> y -> Many (Append xs '[y])
postfix (Many lo lm) y = Many lo
    (M.insert (rightKeyForSnoc (LeftOffset lo) (LeftSize (M.size lm)) (RightOffset 0) (Key 0))
        (unsafeCoerce y)
        lm)
infixl 5 `postfix`

-- | 'snoc' mnemonic: Many is larger '\.' than the smaller element
(\.) :: Many xs -> y -> Many (Append xs '[y])
(\.) = postfix
infixl 5 \.

-- | 'append' mnemonic: 'cons' './' with an extra slash (meaning 'Many') in front.
(/./) :: Many xs -> Many ys -> Many (Append xs ys)
(/./) = append
infixr 5 /./ -- like (++)

-- | Appends two Manys together
append :: Many xs -> Many ys -> Many (Append xs ys)
append (Many lo lm) (Many ro rm) = if ld >= rd
    then Many
         lo
         (lm `M.union` (M.mapKeys (rightKeyForSnoc (LeftOffset lo) (LeftSize ld) (RightOffset ro)) rm))
    else Many
         (unNewRightOffset nro)
         ((M.mapKeys (leftKeyForCons (LeftOffset lo) nro) lm) `M.union` rm)
  where
    ld = M.size lm
    rd = M.size rm
    nro = rightOffsetForCons (LeftSize ld) (RightOffset ro)
infixr 5 `append` -- like Data.List (++)

-----------------------------------------------------------------------

-- | Extract the first element of a Many, which guaranteed to be non-empty.
-- Analogous to 'Partial.head'
front :: Many (x ': xs) -> x
front (Many _ m) = unsafeCoerce (snd . Partial.head $ M.toAscList m)

-- | Extract the 'back' element of a Many, which guaranteed to be non-empty.
-- Analogous to 'Prelude.last'
back :: Many (x ': xs) -> Last (x ': xs)
back (Many _ m) = unsafeCoerce (snd . Partial.head $ M.toDescList m)

-- | Extract the elements after the front of a Many, which guaranteed to be non-empty.
-- Analogous to 'Partial.tail'
aft :: Many (x ': xs) -> Many xs
aft (Many o m) = Many (o + 1) (M.delete (Key o) m)

-- | Return all the elements of a Many except the 'back' one, which guaranteed to be non-empty.
-- Analogous to 'Prelude.init'
fore :: Many (x ': xs) -> Many (Init (x ': xs))
fore (Many o m) = Many o (M.delete (Key (o + M.size m - 1)) m)

--------------------------------------------------

-- | Getter. Use TypeApplication of the type to get
-- Only available for 'Many' with 'IsDistinct' xs.
--
-- @fetch \@Int t@
fetch :: forall x xs. UniqueMember x xs => Many xs -> x
fetch (Many o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix version of 'fetch', with a extra proxy to carry the destination type.
--
-- @foo .^. (Proxy \@Int)@
--
-- Mnemonic: Like 'Control.Lens.(^.)' but with an extra @.@ in front.
(.^.) :: forall x xs proxy. UniqueMember x xs => Many xs -> proxy x -> x
(.^.) v _ = fetch v
infixl 8 .^. -- like Control.Lens.(^.)

--------------------------------------------------

-- | Getter. Get the value of the field at index type-level Nat @n@
--
-- @getchN (Proxy \@2) t@
fetchN :: forall n x xs proxy. MemberAt n x xs => proxy n -> Many xs -> x
fetchN p (Many o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal p)

--------------------------------------------------

-- | Setter. Use TypeApplication of the type to set.
--
-- @replace \@Int t@
--
replace :: forall x xs. UniqueMember x xs => Many xs -> x -> Many xs
replace (Many o m) v = Many o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix version of 'replace'
-- Only available for 'Many' with 'IsDistinct' xs.
--
-- @foo ..~ x@
--
-- Mnemonic: Like a back to front 'Control.Lens.(.~)' with an extra @.@ in front.
(.~.) :: forall x xs. UniqueMember x xs => Many xs -> x -> Many xs
(.~.) = replace
infixl 1 .~. -- like Control.Lens.(.~)

--------------------------------------------------

-- | Setter. Use TypeApplication of the Nat index to set.
--
-- @replaceN (Proxy \@1) t@
--
replaceN :: forall n x xs proxy. MemberAt n x xs => proxy n -> Many xs -> x -> Many xs
replaceN p (Many o m) v = Many o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal p)

-----------------------------------------------------------------------

-- | 'fetch' and 'replace' in lens form.
-- Use TypeApplication to specify the field type of the lens.
-- Example: @item \@Int@
item :: forall x xs. UniqueMember x xs => Lens' (Many xs) x
item = lens fetch replace
{-# INLINE item #-}

-- | 'fetchN' and 'replaceN' in lens form.
-- You can use TypeApplication to specify the index of the type of the lens.
-- Example: @itemN \@1 Proxy@
itemN ::  forall n x xs proxy. MemberAt n x xs => proxy n -> Lens' (Many xs) x
itemN p = lens (fetchN p) (replaceN p)
{-# INLINE itemN #-}

-----------------------------------------------------------------------

-- | Internal function for construction - do not expose!
fromList' :: Ord k => [(k, WrappedAny)] -> M.Map k Any
fromList' xs = M.fromList (coerce xs)

-- | Wraps a 'Case' into an instance of 'Emit', feeding 'Case' with the value from the Many and 'emit'ting the results.
-- Internally, this holds the left-over [(k, v)] from the original Many with the remaining typelist xs.
-- That is the first v in the (k, v) is of type x, and the length of the list is equal to the length of xs.
newtype Via c (xs :: [Type]) r = Via (c xs r, [Any])

-- | Creates an 'Via' safely, so that the invariant of \"typelist to the value list type and size\" holds.
via :: c xs r -> Many xs -> Via c xs r
via c (Many _ m) = Via (c, snd <$> M.toAscList m)

instance Reiterate c (x ': xs) => Reiterate (Via c) (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (Via (c, xxs)) = Via (reiterate c, Partial.tail xxs)

instance (Case c (x ': xs) r) => Emit (Via c) (x ': xs) r where
    emit (Via (c, xxs)) = case' c (unsafeCoerce v)
      where
       -- use of front here is safe as we are guaranteed the length from the typelist
       v = Partial.head xxs

-- | Destruction for any 'Many', even with indistinct types.
-- Given a distinct handler for the fields in 'Many', create a 'Collector'
-- of the results of running the handler over the 'Many'.
-- The 'Collector' is 'AFoldable' to get the results.
forMany :: c xs r -> Many xs -> Collector (Via c) xs r
forMany c x = Collector (via c x)

-- | This is @flip forMany@
collect :: Many xs -> c xs r -> Collector (Via c) xs r
collect = flip forMany

-----------------------------------------------------------------------

newtype ViaN c (n :: Nat) (xs :: [Type]) r = ViaN (c n xs r, [Any])

-- | Creates an 'ViaN' safely, so that the invariant of \"typelist to the value list type and size\" holds.
viaN :: c n xs r -> Many xs -> ViaN c n xs r
viaN c (Many _ m) = ViaN (c, snd <$> M.toAscList m)

instance ReiterateN c n (x ': xs) => ReiterateN (ViaN c) n (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterateN (ViaN (c, xxs)) = ViaN (reiterateN c, Partial.tail xxs)

instance (Case (c n) (x ': xs) r) => Emit (ViaN c n) (x ': xs) r where
    emit (ViaN (c, xxs)) = case' c (unsafeCoerce v)
      where
       -- use of front here is safe as we are guaranteed the length from the typelist
       v = Partial.head xxs

forManyN :: c n xs r -> Many xs -> CollectorN (ViaN c) n xs r
forManyN c x = CollectorN (viaN c x)

-- | This is @flip forManyN@
collectN :: Many xs -> c n xs r -> CollectorN (ViaN c) n xs r
collectN = flip forManyN

-----------------------------------------------------------------------

type Narrow (smaller :: [Type]) (larger :: [Type]) =
    (AFoldable
        ( Collector (Via (CaseNarrow smaller)) larger) [(Key, WrappedAny)]
        , IsDistinct smaller)

-- | Construct a 'Many' with a smaller number of fields than the original
-- Analogous to 'fetch' getter but for multiple fields
-- Specify a typelist of fields to 'narrow' into.
--
-- @narrow \@[Int,Bool] t@
narrow :: forall smaller larger. Narrow smaller larger => Many larger -> Many smaller
narrow t = Many 0 (fromList' xs')
  where
    xs' = afoldr (++) [] (forMany (CaseNarrow @smaller @larger) t)

-- | infix version of 'narrow', with a extra proxy to carry the @smaller@ type.
--
-- @foo \^. (Proxy @'[Int, Bool])@
--
-- Mnemonic: Like 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
(\^.) :: forall smaller larger proxy. Narrow smaller larger => Many larger -> proxy smaller -> Many smaller
(\^.) t _ = narrow t
infixl 8 \^. -- like Control.Lens.(^.)

-- | For each type x in @larger@, generate the (k, v) in @smaller@ (if it exists)
data CaseNarrow (smaller :: [Type]) (xs :: [Type]) r = CaseNarrow

instance Reiterate (CaseNarrow smaller) (x ': xs) where
    reiterate CaseNarrow = CaseNarrow

-- | For each type x in larger, find the index in ys, and create an (incrementing key, value)
instance forall smaller x xs. MaybeUniqueMember x smaller =>
         Case (CaseNarrow smaller) (x ': xs) [(Key, WrappedAny)] where
    case' _ v =
        case i of
            0 -> []
            i' -> [(Key (i' - 1), WrappedAny (unsafeCoerce v))]
      where
        i = fromInteger (natVal @(PositionOf x smaller) Proxy)

-----------------------------------------------------------------------

type NarrowN (ns :: [Nat]) (smaller ::[Type]) (larger :: [Type]) =
    ( AFoldable (CollectorN (ViaN (CaseNarrowN ns smaller)) 0 larger) [(Key, WrappedAny)]
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns)

-- | Construct a 'Many' with a smaller number of fields than the original
-- Analogous to 'fetchN' getter but for multiple fields
-- Specify a Nat-list mapping to 'narrowN' into,
-- where indices[smaller_idx] = larger_idx
--
-- @narrowN \@[6,2] Proxy t@
narrowN
    :: forall ns smaller larger proxy.
       NarrowN ns smaller larger
    => proxy ns -> Many larger -> Many smaller
narrowN _ xs = Many 0 (fromList' xs')
  where
    xs' = afoldr (++) [] (forManyN (CaseNarrowN @ns @smaller @0 @larger) xs)

data CaseNarrowN (indices :: [Nat]) (smaller :: [Type]) (n :: Nat) (xs :: [Type]) r = CaseNarrowN

instance ReiterateN (CaseNarrowN indices smaller) n (x ': xs) where
    reiterateN CaseNarrowN = CaseNarrowN

-- | For each type x in @larger@, find the index in ys, and create an (incrementing key, value)
instance forall indices smaller n x xs. MaybeMemberAt (PositionOf n indices) x smaller =>
         Case (CaseNarrowN indices smaller n) (x ': xs) [(Key, WrappedAny)] where
    case' _ v =
        case i of
            0 -> []
            i' -> [(Key (i' - 1), WrappedAny (unsafeCoerce v))]
      where
        i = fromInteger (natVal @(PositionOf n indices) Proxy)

-----------------------------------------------------------------------

type Amend smaller larger = (AFoldable (Collector (Via (CaseAmend larger)) smaller) (Key, WrappedAny)
       , IsDistinct smaller)

-- | Sets the subset of  'Many' in the larger 'Many'
-- Analogous to 'replace' setter but for multiple fields.
-- Specify a typelist of fields to 'amend'.
--
-- @amend \@[Int,Bool] t1 t2@
amend :: forall smaller larger. Amend smaller larger => Many larger -> Many smaller -> Many larger
amend (Many lo lm) t = Many lo (fromList' xs' `M.union` lm)
  where
    xs' = afoldr (:) [] (forMany (CaseAmend @larger @smaller lo) t)

-- | infix version of 'amend'. Mnemonic: Like 'Control.Lens.(.~)' but with an extra '\' (narrow to the right) in front.
--
-- @t1 \.~ t2
--
-- Mnemonic: Like backwards 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
(\~.) :: forall smaller larger. Amend smaller larger => Many larger -> Many smaller -> Many larger
(\~.) = amend
infixl 1 \~. -- like Control.Lens.(.~)

newtype CaseAmend (larger :: [Type]) (xs :: [Type]) r = CaseAmend Int

instance Reiterate (CaseAmend larger) (x ': xs) where
    reiterate (CaseAmend lo) = CaseAmend lo

-- | for each x in @smaller@, convert it to a (k, v) to insert into the x in @Many larger@
instance UniqueMember x larger => Case (CaseAmend larger) (x ': xs) (Key, WrappedAny) where
    case' (CaseAmend lo) v = (Key (lo + i), WrappedAny (unsafeCoerce v))
      where
        i = fromInteger (natVal @(IndexOf x larger) Proxy)

-----------------------------------------------------------------------

type AmendN ns smaller larger =
    ( AFoldable (CollectorN (ViaN (CaseAmendN ns larger)) 0 smaller) (Key, WrappedAny)
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns)

-- | Sets the subset of  'Many' in the larger 'Many'
-- Analogous to 'replaceN' setter but for multiple fields.
-- Specify a Nat-list mapping to 'amendN' into,
-- where indices[smaller_idx] = larger_idx
--
-- @amendN \@[6,2] Proxy t1 t2@
amendN :: forall ns smaller larger proxy.
       (AmendN ns smaller larger)
    => proxy ns -> Many larger -> Many smaller -> Many larger
amendN _ (Many lo lm) t = Many lo (fromList' xs' `M.union` lm)
  where
    xs' = afoldr (:) [] (forManyN (CaseAmendN @ns @larger @0 @smaller lo) t)

newtype CaseAmendN (indices :: [Nat]) (larger :: [Type]) (n :: Nat) (xs :: [Type]) r = CaseAmendN Int

instance ReiterateN (CaseAmendN indices larger) n (x ': xs) where
    reiterateN (CaseAmendN lo) = CaseAmendN lo

-- | for each x in @smaller@, convert it to a (k, v) to insert into the x in @larger@
instance (MemberAt (KindAtIndex n indices) x larger) =>
         Case (CaseAmendN indices larger n) (x ': xs) (Key, WrappedAny) where
    case' (CaseAmendN lo) v = (Key (lo + i), WrappedAny (unsafeCoerce v))
      where
        i = fromInteger (natVal @(KindAtIndex n indices) Proxy)

-----------------------------------------------------------------------

-- | Projection.
-- A Many can be narrowed or have its order changed by projecting into another Many type.
--
-- @project = lens narrow amend@
--
-- Use TypeApplication to specify the @smaller@ typelist of the lens.
--
-- @project \@'[Int, String]@
--
-- Use @_ to specify the @larger@ typelist instead.
--
-- @project \@_ \@'[Int, String]@
project
    :: forall smaller larger.
       (Narrow smaller larger, Amend smaller larger)
    => Lens' (Many larger) (Many smaller)
project = lens narrow amend
{-# INLINE project #-}

-- | Projection.
-- A version of 'project' using a Nat-list to specify the other Many type.
--
-- @projectN p = lens (narrowN p) (amendN p)@
--
-- Specify a Nat-list mapping of the indicies of the original fields.
--
-- @projectN (Proxy \@'[Int, String])@
--
projectN
    :: forall ns smaller larger proxy.
       (NarrowN ns smaller larger, AmendN ns smaller larger)
    => proxy ns -> Lens' (Many larger) (Many smaller)
projectN p = lens (narrowN p) (amendN p)
{-# INLINE projectN #-}

-----------------------------------------------------------------------

-- | Stores the left & right Many and a list of Any which must be the same length and types in xs typelist.
newtype EmitEqMany (xs :: [Type]) r = EmitEqMany ([Any], [Any])

instance Reiterate EmitEqMany (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitEqMany (ls, rs)) = EmitEqMany (Partial.tail ls, Partial.tail rs)

instance Eq x => Emit EmitEqMany (x ': xs) Bool where
    emit (EmitEqMany (ls, rs)) = l == r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

eqMany
    :: forall xs.
       AFoldable (Collector EmitEqMany xs) Bool
    => Many xs -> Many xs -> [Bool]
eqMany (Many _ lm) (Many _ rm) = afoldr (:) []
    (Collector (EmitEqMany @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance AFoldable (Collector EmitEqMany xs) Bool => Eq (Many xs) where
    lt == rt = foldr (\e z -> bool False z e) True eqs
      where
        eqs = eqMany lt rt

-----------------------------------------------------------------------

-- | Stores the left & right Many and a list of Any which must be the same length and types in xs typelist.
newtype EmitOrdMany (xs :: [Type]) r = EmitOrdMany ([Any], [Any])

instance Reiterate EmitOrdMany (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitOrdMany (ls, rs)) = EmitOrdMany (Partial.tail ls, Partial.tail rs)

instance Ord x => Emit EmitOrdMany (x ': xs) Ordering where
    emit (EmitOrdMany (ls, rs)) = compare l r
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        l = unsafeCoerce (Partial.head ls) :: x
        r = unsafeCoerce (Partial.head rs) :: x

ordMany
    :: forall xs.
       AFoldable (Collector EmitOrdMany xs) Ordering
    => Many xs -> Many xs -> [Ordering]
ordMany (Many _ lm) (Many _ rm) = afoldr (:) []
    (Collector (EmitOrdMany @xs (snd <$> M.toAscList lm, snd <$> M.toAscList rm)))

instance (Eq (Many xs), AFoldable (Collector EmitOrdMany xs) Ordering) => Ord (Many xs) where
    compare lt rt = foldr (\o z -> case o of
                                       EQ -> z
                                       o' -> o') EQ ords
      where
        ords = ordMany lt rt

-----------------------------------------------------------------------

-- | Internally uses [Any] like Via, except also handle the empty type list.
newtype EmitShowMany (xs :: [Type]) r = EmitShowMany [Any]

instance Reiterate EmitShowMany (x ': xs) where
    -- use of tail here is safe as we are guaranteed the length from the typelist
    reiterate (EmitShowMany xxs) = EmitShowMany (Partial.tail xxs)

instance Emit EmitShowMany '[] ShowS where
    emit _ = showString "nul"


instance Show x => Emit EmitShowMany (x ': xs) ShowS where
    emit (EmitShowMany xxs) = showsPrec (cons_prec + 1) v . showString " ./ "
      where
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xxs) :: x
        cons_prec = 5 -- infixr 5 cons

showMany
    :: forall xs.
       AFoldable (Collector0 EmitShowMany xs) ShowS
    => Many xs -> ShowS
showMany (Many _ m) = afoldr (.) id (Collector0 (EmitShowMany @xs (snd <$> M.toAscList m)))

instance AFoldable (Collector0 EmitShowMany xs) ShowS => Show (Many xs) where
    showsPrec d t = showParen (d > cons_prec) $ showMany t
      where
        cons_prec = 5 -- infixr 5 cons

-----------------------------------------------------------------------

newtype EmitReadMany (xs :: [Type]) r = EmitReadMany Key

instance Reiterate EmitReadMany (x ': xs) where
    reiterate (EmitReadMany (Key i)) = EmitReadMany (Key (i + 1))

instance Emit EmitReadMany '[] (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadMany _) = do
        lift $ L.expect (Ident "nul")
        pure []

instance Read x => Emit EmitReadMany (x ': xs) (ReadPrec [(Key, WrappedAny)]) where
    emit (EmitReadMany i) = do
        a <- readPrec @x
        lift $ L.expect (Symbol "./")
        pure [(i, WrappedAny (unsafeCoerce a))]

readMany
    :: forall xs.
       AFoldable (Collector0 EmitReadMany xs) (ReadPrec [(Key, WrappedAny)])
    => Proxy (xs :: [Type]) -> ReadPrec [(Key, WrappedAny)]
readMany _ = afoldr (liftA2 (++)) (pure []) (Collector0 (EmitReadMany @xs (Key 0)))

instance (AFoldable (Collector0 EmitReadMany xs) (ReadPrec [(Key, WrappedAny)])) =>
         Read (Many xs) where
    readPrec =
        parens $
        prec 10 $ do
            xs <- readMany @xs Proxy
            pure (Many 0 (fromList' xs))

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any
