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
    -- ** By Nat index offset
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
-- These usages in this module are safe due to size guarantees provided by the typelist.
import Prelude as Partial

newtype Key = Key Int deriving (Eq, Ord, Show)
newtype LeftOffset = LeftOffset Int
newtype LeftSize = LeftSize Int
newtype RightOffset = RightOffset Int
newtype NewRightOffset = NewRightOffset { unNewRightOffset :: Int }

-- | A Many is an anonymous product type (also know as polymorphic record), with no limit on the number of fields.
--
-- The following functions are available can be used to manipulate unique fields
--
-- * getter/setter for single field: 'fetch' and 'replace'
-- * getter/setter for multiple fields: 'narrow' and 'amend'
-- * folds: 'forMany' or 'collect'
--
-- These functions are type specified. This means labels are not required because the types themselves can be used to access the 'Many.
-- It is a compile error to use those functions for duplicate fields.
--
-- For duplicate fields, Nat-indexed versions of the functions are available:
--
-- * getter/setter for single field: 'fetchN' and 'replaceN'
-- * getter/setter for multiple fields: 'narrowN' and 'amendN'
-- * folds: 'forManyN' or 'collectN'
--
-- Encoding: The record is encoded as (Offset, Map Int Any).
-- This encoding should reasonabily efficient for any number of fields.
--
-- The map Key is index + offset of the type in the typelist.
-- The Offset is used to allow efficient cons 'prefix'.
--
-- @Key = Index of type in typelist + Offset@
--
-- The constructor will guarantee the correct number and types of the elements.
-- The constructor is only exported in the "Data.Diverse.Many.Internal" module
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Int (M.Map Key Any)

-- | Inferred role is phantom which is incorrect
type role Many representational

-----------------------------------------------------------------------

-- | This instance allows converting to and from Many
-- There are instances for converting tuples of up to size 15.
class IsMany t xs a where
    toMany :: t xs a -> Many xs
    fromMany :: Many xs -> t xs a

-- | Converts from a value (eg a tuple) to a 'Many', via a 'Tagged' wrapper
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

-- | These instances add about 7 seconds to the compile time!
instance IsMany Tagged '[] () where
    toMany _ = nul
    fromMany _ = Tagged ()

-- | This single field instance is the reason for 'Tagged' wrapper.
-- Otherwise this instance will overlap.
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
infixr 5 `nul` -- to be the same as 'prefix'

-- | Create a Many from a single value. Analogous to 'M.singleton'
single :: x -> Many '[x]
single v = Many 0 (M.singleton (Key 0) (unsafeCoerce v))

-- | Add an element to the left of a Many.
-- Not named @cons@ to avoid conflict with 'Control.Lens.cons'
prefix :: x -> Many xs -> Many (x ': xs)
prefix x (Many ro rm) = Many (unNewRightOffset nro)
    (M.insert
        (leftKeyForCons (LeftOffset 0) nro (Key 0))
        (unsafeCoerce x)
        rm)
  where
    nro = rightOffsetForCons (LeftSize 1) (RightOffset ro)
infixr 5 `prefix`

-- | Infix version of 'prefix'.
--
-- Mnemonic: Element on the left is smaller './' than the larger 'Many' to the right.
(./) :: x -> Many xs -> Many (x ': xs)
(./) = prefix
infixr 5 ./ -- like Data.List.(:)

-- | Add an element to the right of a Many
-- Not named 'snoc' to avoid conflict with 'Control.Lens.snoc'
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

-- | Getter by unique type. Get the field with type @x@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- 'fetch' \@Int x \`shouldBe` 5
-- @
fetch :: forall x xs. UniqueMember x xs => Many xs -> x
fetch (Many o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix version of 'fetch', with a extra proxy to carry the destination type.
--
-- Mnemonic: Like 'Control.Lens.(^.)' but with an extra @.@ in front.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- x '.^.' (Proxy \@Int) \`shouldBe` 5
-- @
(.^.) :: forall x xs proxy. UniqueMember x xs => Many xs -> proxy x -> x
(.^.) v _ = fetch v
infixl 8 .^. -- like Control.Lens.(^.)

--------------------------------------------------

-- | Getter by index. Get the value of the field at index type-level Nat @n@
--
-- @getchN (Proxy \@2) t@
fetchN :: forall n x xs proxy. MemberAt n x xs => proxy n -> Many xs -> x
fetchN p (Many o m) = unsafeCoerce (m M.! (Key (o + i)))
  where i = fromInteger (natVal p)

--------------------------------------------------

-- | Setter by unique type. Set the field with type @x@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- 'replace' \@Int x 6 \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- @
replace :: forall x xs. UniqueMember x xs => Many xs -> x -> Many xs
replace (Many o m) v = Many o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal @(IndexOf x xs) Proxy)

-- | infix version of 'replace'
--
-- Mnemonic: Like a back to front 'Control.Lens.(.~)' with an extra @.@ in front.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- (x '.~.' (6 :: Int)) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- @
(.~.) :: forall x xs. UniqueMember x xs => Many xs -> x -> Many xs
(.~.) = replace
infixl 1 .~. -- like Control.Lens.(.~)

--------------------------------------------------

-- | Setter by index. Set the value of the field at index type-level Nat @n@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- 'replaceN' \@0 Proxy x 7 `shouldBe`
-- @
replaceN :: forall n x xs proxy. MemberAt n x xs => proxy n -> Many xs -> x -> Many xs
replaceN p (Many o m) v = Many o (M.insert (Key (o + i)) (unsafeCoerce v) m)
  where i = fromInteger (natVal p)

-----------------------------------------------------------------------

-- | 'fetch' ('view' 'item') and 'replace' ('set' 'item') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- x '^.' 'item' \@Int \`shouldBe` 5
-- (x '&' 'item' \@Int .~ 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- @
item :: forall x xs. UniqueMember x xs => Lens' (Many xs) x
item = lens fetch replace
{-# INLINE item #-}

-- | 'fetchN' ('view' 'item') and 'replaceN' ('set' 'item') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' ./ nul
-- x '^.' 'itemN' (Proxy \@0) \`shouldBe` 5
-- (x '&' 'itemN' (Proxy @0) '.~' 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- @
itemN ::  forall n x xs proxy. MemberAt n x xs => proxy n -> Lens' (Many xs) x
itemN p = lens (fetchN p) (replaceN p)
{-# INLINE itemN #-}

-----------------------------------------------------------------------

-- | Internal function for construction - do not expose!
fromList' :: Ord k => [(k, WrappedAny)] -> M.Map k Any
fromList' xs = M.fromList (coerce xs)

-- | Wraps a 'Case' into an instance of 'Emit', 'reiterate'ing and feeding 'Case' with the value from the 'Many'
-- and 'emit'ting the results.
--
-- Internally, this holds the left-over [(k, v)] from the original 'Many' for the remaining typelist @xs@.
--
-- That is the first v in the (k, v) is of type @x@, and the length of the list is equal to the length of @xs@.
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

-- | Folds any 'Many', even with indistinct types.
-- Given __distinct__ handlers for the fields in 'Many', create a 'Collector'
-- of the results of running the handlers over the fields in 'Many'.
--
-- The 'Collector' is 'AFoldable' to combine the results.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Char './' show \@(Maybe Char) './' show \@Bool './' 'nul'
-- 'afoldr' (:) [] ('forMany' ('Data.Diverse.Cases.cases' y) x) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
forMany :: c xs r -> Many xs -> Collector (Via c) xs r
forMany c x = Collector (via c x)

-- | This is @flip 'forMany'@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Char './' show \@(Maybe Char) './' show \@Bool './' 'nul'
-- 'afoldr' (:) [] ('collect' x ('Data.Diverse.Cases.cases' y)) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
collect :: Many xs -> c xs r -> Collector (Via c) xs r
collect = flip forMany

-----------------------------------------------------------------------

-- | A variation of 'Via' which __'reiterateN'__ instead.
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

-- | Folds any 'Many', even with indistinct types.
-- Given __index__ handlers for the fields in 'Many', create a 'CollectorN'
-- of the results of running the handlers over the fields in 'Many'.
--
-- The 'CollectorN' is 'AFoldable' to combine the results.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Bool './' show \@Char './' show \@(Maybe Char) './' show \@Int './' show \@(Maybe Char) './' 'nul'
-- 'afoldr' (:) [] ('forManyN' ('Data.Diverse.Cases.casesN' y) x) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
forManyN :: c n xs r -> Many xs -> CollectorN (ViaN c) n xs r
forManyN c x = CollectorN (viaN c x)

-- | This is @flip 'forManyN'@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
--     y = show \@Int './' show \@Bool './' show \@Char './' show \@(Maybe Char) './' show \@Int './' show \@(Maybe Char) './' 'nul'
-- 'afoldr' (:) [] ('collectN' x ('Data.Diverse.Cases.casesN' y)) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
collectN :: Many xs -> c n xs r -> CollectorN (ViaN c) n xs r
collectN = flip forManyN

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'narrow'
type Narrow (smaller :: [Type]) (larger :: [Type]) =
    (AFoldable
        ( Collector (Via (CaseNarrow smaller larger)) larger) [(Key, WrappedAny)])

-- | Construct a 'Many' with a smaller number of fields than the original.
-- Analogous to 'fetch' getter but for multiple fields.
--
-- This can also be used to reorder fields in the original 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- 'narrow' \@'[Bool, Char] x \`shouldBe` False './' \'X' './' 'nul'
-- @
narrow :: forall smaller larger. Narrow smaller larger => Many larger -> Many smaller
narrow t = Many 0 (fromList' xs')
  where
    xs' = afoldr (++) [] (forMany (CaseNarrow @smaller @larger @larger) t)

-- | infix version of 'narrow', with a extra proxy to carry the @smaller@ type.
--
-- Mnemonic: Like 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- x '\^.' (Proxy @'[Bool, Char]) \`shouldBe` False './' \'X' './' 'nul'
-- @
(\^.) :: forall smaller larger proxy. Narrow smaller larger => Many larger -> proxy smaller -> Many smaller
(\^.) t _ = narrow t
infixl 8 \^. -- like Control.Lens.(^.)

-- | For each type x in @larger@, generate the (k, v) in @smaller@ (if it exists)
data CaseNarrow (smaller :: [Type]) (larger :: [Type]) (xs :: [Type]) r = CaseNarrow

instance Reiterate (CaseNarrow smaller larger) (x ': xs) where
    reiterate CaseNarrow = CaseNarrow

-- | For each type x in larger, find the index in ys, and create an (incrementing key, value)
instance forall smaller larger x xs. (UniqueIfExists smaller x larger, MaybeUniqueMember x smaller) =>
         Case (CaseNarrow smaller larger) (x ': xs) [(Key, WrappedAny)] where
    case' _ v =
        case i of
            0 -> []
            i' -> [(Key (i' - 1), WrappedAny (unsafeCoerce v))]
      where
        i = fromInteger (natVal @(PositionOf x smaller) Proxy)

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'narrowN'
type NarrowN (ns :: [Nat]) (smaller ::[Type]) (larger :: [Type]) =
    ( AFoldable (CollectorN (ViaN (CaseNarrowN ns smaller)) 0 larger) [(Key, WrappedAny)]
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns)

-- | A variation of 'narrow' which uses a Nat list @n@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx@
-- @
--
-- This variation allows @smaller@ or @larger@ to contain indistinct since
-- the mapping is specified by @indicies@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- 'narrowN' (Proxy @'[5, 4, 0]) x \`shouldBe` Just \'A' './' (6 :: Int) './' (5 ::Int) './' 'nul'
-- @
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

-- | A friendlier type constraint synomyn for 'amend'
type Amend smaller larger = (AFoldable (Collector (Via (CaseAmend larger)) smaller) (Key, WrappedAny)
       , IsDistinct smaller)

-- | Sets the subset of 'Many' in the larger 'Many'.
-- Analogous to 'replace' setter but for multiple fields.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- 'amend' \@'[Int, Maybe Char] x ((6 :: Int) './' Just \'P' './' 'nul') \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nul'
-- @
amend :: forall smaller larger. Amend smaller larger => Many larger -> Many smaller -> Many larger
amend (Many lo lm) t = Many lo (fromList' xs' `M.union` lm)
  where
    xs' = afoldr (:) [] (forMany (CaseAmend @larger @smaller lo) t)

-- | infix version of 'amend'. Mnemonic: Like 'Control.Lens.(.~)' but with an extra '\' (narrow to the right) in front.
--
-- Mnemonic: Like backwards 'Control.Lens.(^.)' but with an extra '\' (narrow to the right) in front.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- (x '\~.' (6 :: Int) './' Just \'P' './' 'nul') \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nul'
-- @
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

-- | A friendlier type constraint synomyn for 'amendN'
type AmendN ns smaller larger =
    ( AFoldable (CollectorN (ViaN (CaseAmendN ns larger)) 0 smaller) (Key, WrappedAny)
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns)

-- | A variation of 'amend' which uses a Nat list @n@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx@
-- @
--
-- This variation allows @smaller@ or @larger@ to contain indistinct since
-- the mapping is specified by @indicies@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- 'amendN' (Proxy \@'[5, 4, 0]) x (Just \'B' './' (8 :: Int) './' (4 ::Int) './' 'nul') \`shouldBe`
--     (4 :: Int) './' False './' \'X' './' Just \'O' './' (8 :: Int) './' Just \'B' './' 'nul'
-- @
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

-- | 'narrow' ('view' 'project') and 'amend' ('set' 'project') in 'Lens'' form.
--
-- @
-- 'project' = 'lens' 'narrow' 'amend'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'
-- x '^.' ('project' \@'[Int, Maybe Char]) \`shouldBe` (5 :: Int) './' Just \'O' './' 'nul'
-- (x '&' ('project' \@'[Int, Maybe Char]) '.~' ((6 :: Int) './' Just 'P' './' 'nul')) \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nul'
-- @
project
    :: forall smaller larger.
       (Narrow smaller larger, Amend smaller larger)
    => Lens' (Many larger) (Many smaller)
project = lens narrow amend
{-# INLINE project #-}

-- | 'narrowN' ('view' 'projectN') and 'amendN' ('set' 'projectN') in 'Lens'' form.
--
-- @
-- 'projectN' = 'lens' 'narrowN' 'amendN'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nul'
-- x '^.' ('projectN' \@'[5, 4, 0] Proxy) \`shouldBe` Just \'A' './' (6 :: Int) './' (5 ::Int) './' 'nul'
-- (x '&' ('projectN' \@'[5, 4, 0] Proxy) '.~' (Just \'B' './' (8 :: Int) './' (4 ::Int) './' nul)) \`shouldBe`
--     (4 :: Int) './' False './' \'X' './' Just \'O' './' (8 :: Int) './' Just \'B' './' 'nul'
-- @
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

-- | Two 'Many's are equal if all their fields equal
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

-- | Two 'Many's are ordered by 'compare'ing their fields in index order
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

-- | @read "5 ./ False ./ 'X' ./ Just 'O' ./ nul" == (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'@
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

-- | @read "5 ./ False ./ 'X' ./ Just 'O' ./ nul" == (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nul'@
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
