{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

      -- * Construction
    , nil
    , single
    , prefix
    , (./)
    , postfix
    , (\.)
    , append
    , (/./)

    -- * Simple queries
    , sliceL
    , sliceR
    , front
    , back
    , aft
    , fore

    -- * Single field
    -- ** Getter for single field
    , fetch
    , fetchL
    , fetchN
    -- ** Setter for single field
    , replace
    , replace'
    , replaceL
    , replaceL'
    , replaceN
    , replaceN'

    -- * Multiple fields
    -- ** Getter for multiple fields
    , Select
    , select
    , selectL
    , SelectN
    , selectN
    -- ** Setter for multiple fields
    , Amend
    , amend
    , Amend'
    , amend'
    , amendL
    , amendL'
    , AmendN
    , amendN
    , AmendN'
    , amendN'

    -- * Destruction
    -- ** By type
    , forMany
    , collect
    -- ** By Nat index offset
    , forManyN
    , collectN
    ) where

import Control.Applicative
import Control.DeepSeq
import Data.Bool
import Data.Diverse.AFoldable
import Data.Diverse.Case
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Foldable
import Data.Kind
import Data.Proxy
import qualified Data.Sequence as S
import Data.Tagged
import qualified GHC.Generics as G
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

-- | A Many is an anonymous product type (also know as polymorphic record), with no limit on the number of fields.
--
-- The following functions are available can be used to manipulate unique fields
--
-- * getter/setter for single field: 'fetch' and 'replace'
-- * getter/setter for multiple fields: 'select' and 'amend'
-- * folds: 'forMany' or 'collect'
--
-- These functions are type specified. This means labels are not required because the types themselves can be used to access the 'Many.
-- It is a compile error to use those functions for duplicate fields.
--
-- For duplicate fields, Nat-indexed versions of the functions are available:
--
-- * getter/setter for single field: 'fetchN' and 'replaceN'
-- * getter/setter for multiple fields: 'selectN' and 'amendN'
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
data Many (xs :: [Type]) = Many (S.Seq Any)

-- Inferred role is phantom which is incorrect
-- representational means:
-- @
-- Coercible '[Int] '[IntLike] => Coercible (Many '[Int]) (Many '[IntLike])
-- @
type role Many representational

-- | Many stored as a list. This is useful when folding over 'Many' efficienty
-- so that the conversion to List is only done once
data Many_ (xs :: [Type]) = Many_ [Any]

type role Many_ representational

toMany_ :: Many xs -> Many_ xs
toMany_ (Many m) = Many_ (toList m)

fromMany_ :: Many_ xs -> Many xs
fromMany_ (Many_ xs) = Many (S.fromList xs)
-----------------------------------------------------------------------

instance NFData (Many '[])
instance (NFData x, NFData (Many xs)) => NFData (Many (x ': xs))

-----------------------------------------------------------------------

-- | A terminating 'G.Generic' instance encoded as a 'nil'.
instance G.Generic (Many '[]) where
    type Rep (Many '[]) =  G.U1
    from _ = {- G.U1 -} G.U1
    to G.U1 = nil

-- | A 'G.Generic' instance encoded as the 'front' value 'G.:*:' with the 'aft' 'Many'.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Many (x ': xs)) where
    type Rep (Many (x ': xs)) = (G.Rec0 x) G.:*: (G.Rec0 (Many xs))
    from r = ({- G.Rec0 -} G.K1 (front r)) G.:*: ({- G.Rec0 -} G.K1 (aft r))
    to (({- G.Rec0 -} G.K1 a) G.:*: ({- G.Rec0 -} G.K1 b)) = a ./ b

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

-- | These instances add about 7 seconds to the compile time!
instance IsMany Tagged '[] () where
    toMany _ = nil
    fromMany _ = Tagged ()

-- | This single field instance is the reason for 'Tagged' wrapper.
-- Otherwise this instance will overlap.
instance IsMany Tagged '[a] a where
    toMany (Tagged a) = single a
    fromMany r = Tagged (fetch @a r)

instance IsMany Tagged '[a,b] (a,b) where
    toMany (Tagged (a,b)) = a./b./nil
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r)

instance IsMany Tagged '[a,b,c] (a,b,c) where
    toMany (Tagged (a,b,c)) = a./b./c./nil
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r)

instance IsMany Tagged '[a,b,c,d] (a,b,c,d) where
    toMany (Tagged (a,b,c,d)) = a./b./c./d./nil
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r)

instance IsMany Tagged '[a,b,c,d,e] (a,b,c,d,e) where
    toMany (Tagged (a,b,c,d,e)) = a./b./c./d./e./nil
    fromMany r = Tagged (fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r)

instance IsMany Tagged '[a,b,c,d,e,f] (a,b,c,d,e,f) where
    toMany (Tagged (a,b,c,d,e,f)) = a./b./c./d./e./f./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r)

instance IsMany Tagged '[a,b,c,d,e,f,g] (a,b,c,d,e,f,g) where
    toMany (Tagged (a,b,c,d,e,f,g)) = a./b./c./d./e./f./g./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h] (a,b,c,d,e,f,g,h) where
    toMany (Tagged (a,b,c,d,e,f,g,h)) = a./b./c./d./e./f./g./h./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i] (a,b,c,d,e,f,g,h,i) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i)) = a./b./c./d./e./f./g./h./i./ nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j] (a,b,c,d,e,f,g,h,i,j) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j)) = a./b./c./d./e./f./g./h./i./j./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k] (a,b,c,d,e,f,g,h,i,j,k) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k)) = a./b./c./d./e./f./g./h./i./j./k./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l] (a,b,c,d,e,f,g,h,i,j,k,l) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l)) = a./b./c./d./e./f./g./h./i./j./k./l./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m] (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m)) = a./b./c./d./e./f./g./h./i./j./k./l./m./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m,n] (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m,n)) = a./b./c./d./e./f./g./h./i./j./k./l./m./n./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r, fetchN (Proxy @13) r)

instance IsMany Tagged '[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toMany (Tagged (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)) = a./b./c./d./e./f./g./h./i./j./k./l./m./n./o./nil
    fromMany r = Tagged ( fetchN (Proxy @0) r, fetchN (Proxy @1) r, fetchN (Proxy @2) r, fetchN (Proxy @3) r, fetchN (Proxy @4) r
                        , fetchN (Proxy @5) r, fetchN (Proxy @6) r, fetchN (Proxy @7) r, fetchN (Proxy @8) r, fetchN (Proxy @9) r
                        , fetchN (Proxy @10) r, fetchN (Proxy @11) r, fetchN (Proxy @12) r, fetchN (Proxy @13) r, fetchN (Proxy @14) r)

-----------------------------------------------------------------------

-- | Analogous to 'Prelude.null'. Named 'nil' to avoid conflicting with 'Prelude.null'.
nil :: Many '[]
nil = Many S.empty

-- | Create a Many from a single value. Analogous to 'M.singleton'
single :: x -> Many '[x]
single v = Many (S.singleton (unsafeCoerce v))

-- | Add an element to the left of a Many.
-- Not named @cons@ to avoid conflict with 'Control.Lens.cons'
prefix :: x -> Many xs -> Many (x ': xs)
prefix x (Many rs) = Many ((unsafeCoerce x) S.<| rs)
infixr 5 `prefix`

prefix' :: x -> Many_ xs -> Many_ (x ': xs)
prefix' x (Many_ xs) = Many_ (unsafeCoerce x : xs)

-- | Infix version of 'prefix'.
--
-- Mnemonic: Element on the left is smaller './' than the larger 'Many' to the right.
(./) :: x -> Many xs -> Many (x ': xs)
(./) = prefix
infixr 5 ./ -- like Data.List.(:)

-- | Add an element to the right of a Many
-- Not named @snoc@ to avoid conflict with 'Control.Lens.snoc'
postfix :: Many xs -> y -> Many (Append xs '[y])
postfix (Many ls) y = Many (ls S.|> (unsafeCoerce y))
infixl 5 `postfix`

-- | Infix version of 'postfix'.
--
-- Mnemonic: Many is larger '\.' than the smaller element
(\.) :: Many xs -> y -> Many (Append xs '[y])
(\.) = postfix
infixl 5 \.

-- | Infix version of 'append'.
--
-- Mnemonic: 'prefix' './' with an extra slash (meaning 'Many') in front.
(/./) :: Many xs -> Many ys -> Many (Append xs ys)
(/./) = append
infixr 5 /./ -- like (++)

-- | Appends two Manys together
append :: Many xs -> Many ys -> Many (Append xs ys)
append (Many ls) (Many rs) = Many (ls S.>< rs)
infixr 5 `append` -- like Data.List (++)

-----------------------------------------------------------------------

-- | Split a non-empty Many into the first element, then the rest of the Many.
-- Analogous to 'S.viewl'
sliceL :: Many (x ': xs) -> (x, Many xs)
sliceL (Many xs) = case S.viewl xs of
    S.EmptyL -> error "no front"
    a S.:< ys -> (unsafeCoerce a, Many ys)

-- | Split a non-empty Many into initial part of Many, and the last element.
-- Analogous to 'S.viewr'
sliceR :: Many (x ': xs) -> (Many (Init (x ': xs)), Last (x ': xs))
sliceR (Many xs) = case S.viewr xs of
    S.EmptyR -> error "no back"
    ys S.:> a -> (Many ys, unsafeCoerce a)

-- | Extract the first element of a Many, which guaranteed to be non-empty.
-- Analogous to 'Partial.head'
front :: Many (x ': xs) -> x
front = fst . sliceL

front' :: Many_ (x ': xs) -> x
front' (Many_ xs) = unsafeCoerce (Partial.head xs)

-- | Extract the 'back' element of a Many, which guaranteed to be non-empty.
-- Analogous to 'Prelude.last'
back :: Many (x ': xs) -> Last (x ': xs)
back = snd . sliceR

-- | Extract the elements after the front of a Many, which guaranteed to be non-empty.
-- Analogous to 'Partial.tail'
aft :: Many (x ': xs) -> Many xs
aft = snd . sliceL

aft' :: Many_ (x ': xs) -> Many_ xs
aft' (Many_ xs) = Many_ (Partial.tail xs)

-- | Return all the elements of a Many except the 'back' one, which guaranteed to be non-empty.
-- Analogous to 'Prelude.init'
fore :: Many (x ': xs) -> Many (Init (x ': xs))
fore = fst . sliceR

--------------------------------------------------

-- | Getter by unique type. Get the field with type @x@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'fetch' \@Int x \`shouldBe` 5
-- @
fetch :: forall x xs. UniqueMember x xs => Many xs -> x
fetch = fetch_

-- | Using S.lookup to ensure Seq is not stored in a thunk
fetch_ :: forall x xs n. (KnownNat n, n ~ IndexOf x xs) => Many xs -> x
fetch_ (Many xs) = let !x = S.index xs i in (unsafeCoerce x) -- forcing x to avoid storing Seq in thunk
  where i = fromInteger (natVal @n Proxy)

--------------------------------------------------

-- | Getter by label. Get the value of the field with tag @label@ which can be any type
-- not just @KnownSymbol@.
--
-- @
-- let y = False './' Tagged \@Foo \'X' './' Tagged @"Hi" True './' 'nil'
-- 'fetchL' \@Foo Proxy y \`shouldBe` Tagged \@Foo \'X'
-- 'fetchL' \@"Hi" Proxy y \`shouldBe` Tagged \@"Hi" True
-- @
fetchL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Many xs -> x
fetchL _ = fetch_ @x

--------------------------------------------------

-- | Getter by index. Get the value of the field at index type-level Nat @n@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'fetchN' @1 Proxy x \`shouldBe` False
-- @
fetchN :: forall n x xs proxy. MemberAt n x xs => proxy n -> Many xs -> x
fetchN p (Many xs) = let !x = S.index xs i in (unsafeCoerce x) -- forcing x to avoid storing Seq in thunk
  where i = fromInteger (natVal p)

--------------------------------------------------

-- | Setter by unique type. Set the field with type @x@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'replace' \@Int x 6 \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- @
replace :: forall x xs. UniqueMember x xs => Many xs -> x -> Many xs
replace = replace_

replace_ :: forall x xs n. (KnownNat n, n ~ IndexOf x xs) => Many xs -> x -> Many xs
replace_ (Many xs) v = Many (S.update i (unsafeCoerce v) xs)
  where i = fromInteger (natVal @n Proxy)

-- | Polymorphic setter by unique type. Set the field with type @x@, and replace with type @y@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'replace'' \@Int Proxy x (Just True) \`shouldBe` Just True './' False './' \'X' './' Just \'O' './' 'nil'
-- @
replace' :: forall x y xs proxy. UniqueMember x xs => proxy x -> Many xs -> y -> Many (Replace x y xs)
replace' = replace'_

replace'_ :: forall x y xs n proxy. (KnownNat n, n ~ IndexOf x xs) => proxy x -> Many xs -> y -> Many (Replace x y xs)
replace'_ _ (Many xs) v = Many (S.update i (unsafeCoerce v) xs)
  where i = fromInteger (natVal @n Proxy)

--------------------------------------------------

-- | Setter by unique label. Set the field with label @l@.
--
-- @
-- let y = (5 :: Int) './' False './' Tagged \@Foo \'X' './' Tagged \@\"Hello" (6 :: Int) './' 'nil'
-- 'replaceL' \@Foo Proxy y (Tagged \@Foo \'Y') \`shouldBe`
--     (5 :: Int) './' False './' Tagged \@Foo \'Y' './' Tagged \@\"Hello" (6 :: Int) './' 'nil'
-- 'replaceL' \@\"Hello" Proxy y (Tagged \@\"Hello" 7) \`shouldBe`
--     (5 :: Int) './' False './' Tagged \@Foo \'X' './' Tagged \@\"Hello" (7 :: Int) './' 'nil'
-- @
replaceL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Many xs -> x -> Many xs
replaceL _ = replace_ @x

-- | Polymorphic setter by unique type. Set the field with type @x@, and replace with type @y@
--
-- @
-- let y = (5 :: Int) './' False './' Tagged \@Foo \'X' './' Tagged \@\"Hello" (6 :: Int) './' 'nil'
-- replaceL' \@Foo Proxy y (Tagged \@Bar \'Y') `shouldBe`
--     (5 :: Int) './' False './' Tagged @Bar 'Y' './' Tagged @"Hello" (6 :: Int) './' 'nil'
-- replaceL' \@\"Hello" Proxy y (Tagged \@\"Hello" False) \`shouldBe`
--     (5 :: Int) './' False './' Tagged \@Foo \'X' './' Tagged \@\"Hello" False './' 'nil'
-- @
replaceL' :: forall l y xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Many xs -> y -> Many (Replace x y xs)
replaceL' _ = replace'_ @x Proxy

--------------------------------------------------

-- | Setter by index. Set the value of the field at index type-level Nat @n@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'replaceN' \@0 Proxy x 7 `shouldBe`
-- @
replaceN :: forall n x y xs proxy. MemberAt n x xs => proxy n -> Many xs -> y -> Many xs
replaceN p (Many xs) v = Many (S.update i (unsafeCoerce v) xs)
  where i = fromInteger (natVal p)

-- | Polymorphic version of 'replaceN'
replaceN' :: forall n x y xs proxy. MemberAt n x xs => proxy n -> Many xs -> y -> Many (ReplaceIndex n y xs)
replaceN' p (Many xs) v = Many (S.update i (unsafeCoerce v) xs)
  where i = fromInteger (natVal p)

-----------------------------------------------------------------------

-- | Internal function for construction - do not expose!
fromList' :: [(Int, WrappedAny)] -> S.Seq Any
fromList' = fmap (\(_, a) -> coerce a) . S.unstableSortBy (\(i, _) (j, _) -> compare i j) . S.fromList

------------------------------------------------------------------------

class CaseAny c (xs :: [Type]) r where
    -- | Return the handler/continuation when x is observed.
    caseAny :: c xs r -> Any -> r

-----------------------------------------------------------------------

-- | Variation of 'Collector' which uses 'CaseAny' instead of 'Case'
data CollectorAny c (xs :: [Type]) r = CollectorAny (c xs r) [Any]

-- | nill case that doesn't even use 'caseAny', so that an instance of @CaseAny '[]@ is not needed.
instance AFoldable (CollectorAny c '[]) r where
    afoldr _ z _ = z

instance ( CaseAny c (x ': xs) r
         , Reiterate c (x ': xs)
         , AFoldable (CollectorAny c xs) r
         ) =>
         AFoldable (CollectorAny c (x ': xs)) r where
    afoldr f z (CollectorAny c xs) = f (caseAny c x) (afoldr f z (CollectorAny (reiterate c) xs'))
      where
       -- use of head/tail here is safe as we are guaranteed the length from the typelist
       x = Partial.head xs
       xs' = Partial.tail xs
    {-# INLINABLE afoldr #-} -- This makes compiling tests a little faster than with no pragma

forMany' :: c xs r -> Many xs -> CollectorAny c xs r
forMany' c (Many xs) = CollectorAny c (toList xs)

-----------------------------------------------------------------------

-- | A variation of 'CollectorN' which uses 'CaseAny' instead of 'Case'
data CollectorAnyN c n (xs :: [Type]) r = CollectorAnyN (c n xs r) [Any]

-- | nill case that doesn't even use 'caseAnyN', so that an instance of @CaseAnyN '[]@ is not needed.
instance AFoldable (CollectorAnyN c n '[]) r where
    afoldr _ z _ = z

instance ( CaseAny (c n) (x ': xs) r
         , ReiterateN c n (x ': xs)
         , AFoldable (CollectorAnyN c (n + 1) xs) r
         ) =>
         AFoldable (CollectorAnyN c n (x ': xs)) r where
    afoldr f z (CollectorAnyN c xs) = f (caseAny c x) (afoldr f z (CollectorAnyN (reiterateN c) xs'))
      where
       -- use of head/tail here is safe as we are guaranteed the length from the typelist
       x = Partial.head xs
       xs' = Partial.tail xs
    {-# INLINABLE afoldr #-} -- This makes compiling tests a little faster than with no pragma

forManyN' :: c n xs r -> Many xs -> CollectorAnyN c n xs r
forManyN' c (Many xs) = CollectorAnyN c (toList xs)

-----------------------------------------------------------------------

-- | Collects the output from 'case''ing each field in a 'Many'.
-- Uses 'Reiterate' to prepare the 'Case' to accept the next type in the @xs@ typelist.
--
--  Internally, this holds the left-over [(k, v)] from the original 'Many' for the remaining typelist @xs@.
--
-- That is, the first v in the (k, v) is of type @x@, and the length of the list is equal to the length of @xs@.
data Collector c (xs :: [Type]) r = Collector (c xs r) [Any]

-- | nill case that doesn't even use 'case'', so that an instance of @Case '[]@ is not needed.
instance AFoldable (Collector c '[]) r where
    afoldr _ z _ = z

-- | Folds values by 'reiterate'ing 'Case's through the @xs@ typelist.
instance ( Case c (x ': xs) r
         , Reiterate c (x ': xs)
         , AFoldable (Collector c xs) r
         ) =>
         AFoldable (Collector c (x ': xs)) r where
    afoldr f z (Collector c xs) = f (case' c v) (afoldr f z (Collector (reiterate c) xs'))
      where
       -- use of head/tail here is safe as we are guaranteed the length from the typelist
       v = unsafeCoerce $ Partial.head xs
       xs' = Partial.tail xs
    {-# INLINABLE afoldr #-} -- This makes compiling tests a little faster than with no pragma

-----------------------------------------------------------------------

-- | Folds any 'Many', even with indistinct types.
-- Given __distinct__ handlers for the fields in 'Many', create 'AFoldable'
-- of the results of running the handlers over the fields in 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
--     y = show \@Int './' show \@Char './' show \@(Maybe Char) './' show \@Bool './' 'nil'
-- 'afoldr' (:) [] ('forMany' ('Data.Diverse.Cases.cases' y) x) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
forMany :: (t ~ Collector c xs, AFoldable t r, Case c xs r) => c xs r -> Many xs -> t r
forMany c (Many xs) = Collector c (toList xs)

-- | This is @flip 'forMany'@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
--     y = show \@Int './' show \@Char './' show \@(Maybe Char) './' show \@Bool './' 'nil'
-- 'afoldr' (:) [] ('collect' x ('Data.Diverse.Cases.cases' y)) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
collect :: (t ~ Collector c xs, AFoldable t r, Case c xs r) => Many xs -> c xs r -> t r
collect = flip forMany

-----------------------------------------------------------------------

-- | A variation of 'Collector' which uses 'ReiterateN' instead of 'Reiterate'
data CollectorN c (n :: Nat) (xs :: [Type]) r = CollectorN (c n xs r) [Any]

-- | nill case that doesn't even use 'case'', so that an instance of @Case '[]@ is not needed.
instance AFoldable (CollectorN c n '[]) r where
    afoldr _ z _ = z

-- | Folds values by 'reiterate'ing 'Emit'ters through the @xs@ typelist.
instance ( Case (c n) (x ': xs) r
         , ReiterateN c n (x ': xs)
         , AFoldable (CollectorN c (n + 1) xs) r
         ) =>
         AFoldable (CollectorN c n (x ': xs)) r where
    afoldr f z (CollectorN c xs) = f (case' c v) (afoldr f z (CollectorN (reiterateN c) xs'))
      where
       -- use of head/tail here is safe as we are guaranteed the length from the typelist
       v = unsafeCoerce $ Partial.head xs
       xs' = Partial.tail xs
    {-# INLINABLE afoldr #-} -- This makes compiling tests a little faster than with no pragma

-- | Folds any 'Many', even with indistinct types.
-- Given __index__ handlers for the fields in 'Many', create 'AFoldable'
-- of the results of running the handlers over the fields in 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
--     y = show \@Int './' show \@Bool './' show \@Char './' show \@(Maybe Char) './' show \@Int './' show \@(Maybe Char) './' 'nil'
-- 'afoldr' (:) [] ('forManyN' ('Data.Diverse.Cases.casesN' y) x) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
forManyN :: (t ~ CollectorN c n xs, AFoldable t r, Case (c n) xs r) => c n xs r -> Many xs -> t r
forManyN c (Many xs) = CollectorN c (toList xs)

-- | This is @flip 'forManyN'@
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
--     y = show \@Int './' show \@Bool './' show \@Char './' show \@(Maybe Char) './' show \@Int './' show \@(Maybe Char) './' 'nil'
-- 'afoldr' (:) [] ('collectN' x ('Data.Diverse.Cases.casesN' y)) \`shouldBe`
--     [\"5", \"False", \"\'X'", \"Just \'O'", \"6", \"Just \'A'"]
-- @
collectN :: (t ~ CollectorN c n xs, AFoldable t r, Case (c n) xs r) => Many xs -> c n xs r -> t r
collectN = flip forManyN

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'select'
type Select (smaller :: [Type]) (larger :: [Type]) =
    (AFoldable
        ( CollectorAny (CaseSelect smaller larger) larger) (Maybe (Int, WrappedAny)))

-- | Construct a 'Many' with a smaller number of fields than the original.
-- Analogous to 'fetch' getter but for multiple fields.
--
-- This can also be used to reorder fields in the original 'Many'.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- 'select' \@'[Bool, Char] x \`shouldBe` False './' \'X' './' 'nil'
-- @
select :: forall smaller larger. Select smaller larger => Many larger -> Many smaller
select t = Many (fromList' xs')
  where
    xs' = afoldr (\a z -> maybe z (: z) a) [] (forMany' (CaseSelect @smaller @larger @larger) t)

-- | For each type x in @larger@, generate the (k, v) in @smaller@ (if it exists)
data CaseSelect (smaller :: [Type]) (larger :: [Type]) (xs :: [Type]) r = CaseSelect

instance Reiterate (CaseSelect smaller larger) (x ': xs) where
    reiterate = coerce

-- | For each type x in larger, find the index in ys, and create a (key, value)
instance forall smaller larger x xs n. (UniqueIfExists smaller x larger, MaybeUniqueMemberAt n x smaller) =>
         CaseAny (CaseSelect smaller larger) (x ': xs) (Maybe (Int, WrappedAny)) where
    caseAny _ v =
        case i of
            0 -> Nothing
            i' -> Just (i' - 1, WrappedAny v)
      where
        i = fromInteger (natVal @n Proxy)

-----------------------------------------------------------------------

-- | A variation of 'select' which selects by labels
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" 'O' './' 'nil'
-- 'selectL' \@'[Foo, Bar] Proxy x \`shouldBe` Tagged \@Foo False './' Tagged \@Bar \'X' './' 'nil'
-- 'selectL' \@'[\"Hi", \"Bye"] Proxy x \`shouldBe` Tagged \@\"Hi" (5 :: Int) './' Tagged \@\"Bye" \'O' './' 'nil'
-- @
selectL
    :: forall ls smaller larger proxy.
       ( Select smaller larger
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger)
    => proxy ls -> Many larger -> Many smaller
selectL _ = select @smaller

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'selectN'
type SelectN (ns :: [Nat]) (smaller ::[Type]) (larger :: [Type]) =
    ( AFoldable (CollectorAnyN (CaseSelectN ns smaller) 0 larger) (Maybe (Int, WrappedAny))
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns)

-- | A variation of 'select' which uses a Nat list @n@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx@
-- @
--
-- This variation allows @smaller@ or @larger@ to contain indistinct since
-- the mapping is specified by @indicies@.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- 'selectN' (Proxy @'[5, 4, 0]) x \`shouldBe` Just \'A' './' (6 :: Int) './' (5 ::Int) './' 'nil'
-- @
selectN
    :: forall ns smaller larger proxy.
       SelectN ns smaller larger
    => proxy ns -> Many larger -> Many smaller
selectN _ xs = Many (fromList' xs')
  where
    xs' = afoldr (\a z -> maybe z (: z) a) [] (forManyN' (CaseSelectN @ns @smaller @0 @larger) xs)

data CaseSelectN (indices :: [Nat]) (smaller :: [Type]) (n :: Nat) (xs :: [Type]) r = CaseSelectN

instance ReiterateN (CaseSelectN indices smaller) n (x ': xs) where
    reiterateN CaseSelectN = CaseSelectN

-- | For each type x in @larger@, find the index in ys, and create an (incrementing key, value)
instance forall indices smaller n x xs n'. (MaybeMemberAt n' x smaller, n' ~ PositionOf n indices) =>
         CaseAny (CaseSelectN indices smaller n) (x ': xs) (Maybe (Int, WrappedAny)) where
    caseAny _ v =
        case i of
            0 -> Nothing
            i' -> Just (i' - 1, WrappedAny v)
      where
        i = fromInteger (natVal @n' Proxy)

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'amend'
type Amend smaller larger = (AFoldable (CollectorAny (CaseAmend larger) smaller) (Int, WrappedAny)
       , IsDistinct smaller)

-- | Sets the subset of 'Many' in the larger 'Many'.
-- Analogous to 'replace' setter but for multiple fields.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- 'amend' \@'[Int, Maybe Char] x ((6 :: Int) './' Just \'P' './' 'nil') \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nil'
-- @
amend :: forall smaller larger. Amend smaller larger => Many larger -> Many smaller -> Many larger
amend (Many ls) t = Many $ foldr (\(i, WrappedAny v) ys -> S.update i v ys) ls xs'
  where
    xs' = afoldr (:) [] (forMany' (CaseAmend @larger @smaller) t)

data CaseAmend (larger :: [Type]) (xs :: [Type]) r = CaseAmend

instance Reiterate (CaseAmend larger) (x ': xs) where
    reiterate = coerce

-- | for each x in @smaller@, convert it to a (k, v) to insert into the x in @Many larger@
instance UniqueMemberAt n x larger => CaseAny (CaseAmend larger) (x ': xs) (Int, WrappedAny) where
    caseAny _ v = (i, WrappedAny v)
      where
        i = fromInteger (natVal @n Proxy)

-----------------------------------------------------------------------

-- | A variation of 'amend' which amends via labels.
--
-- @
-- let x = False ./ Tagged \@\"Hi" (5 :: Int) ./ Tagged \@Foo False ./ Tagged \@Bar \'X' ./ Tagged \@\"Bye" \'O' ./ 'nil'
-- 'amendL' \@'[Foo, Bar] Proxy x (Tagged \@Foo True ./ Tagged \@Bar \'Y' ./ nil) `shouldBe`
--     False ./ Tagged \@\"Hi" (5 :: Int) ./ Tagged \@Foo True ./ Tagged \@Bar \'Y' ./ Tagged \@\"Bye" \'O' ./ 'nil'
-- 'amendL' \@'[\"Hi", \"Bye"] Proxy x (Tagged \@\"Hi" (6 :: Int) ./ Tagged \@\"Bye" \'P' ./ nil) `shouldBe`
--     False ./ Tagged \@\"Hi" (6 :: Int) ./ Tagged \@Foo False ./ Tagged \@Bar \'X' ./ Tagged \@\"Bye" \'P' ./ 'nil'
-- @
amendL
    :: forall ls smaller larger proxy.
       ( Amend smaller larger
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger)
    => proxy ls -> Many larger -> Many smaller -> Many larger
amendL _ = amend @(KindsAtLabels ls larger)

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'amend''
type Amend' smaller smaller' larger zipped =
    ( AFoldable (CollectorAny (CaseAmend' larger) zipped) (Int, WrappedAny)
    , IsDistinct smaller
    , zipped ~ Zip smaller smaller')

amend' :: forall smaller smaller' larger proxy zipped. Amend' smaller smaller' larger zipped
    => proxy smaller -> Many larger -> Many smaller' -> Many (Replaces smaller smaller' larger)
amend' _ (Many ls) t = Many $ foldr (\(i, WrappedAny v) ys -> S.update i v ys) ls xs'
  where
    xs' = afoldr (:) [] (forMany'' @smaller Proxy (CaseAmend' @larger @zipped) t)

forMany'' :: Proxy xs -> c (Zip xs ys) r -> Many ys -> CollectorAny c (Zip xs ys) r
forMany'' _ c (Many ys) = CollectorAny c (toList ys)

data CaseAmend' (larger :: [Type]) (zs :: [Type]) r = CaseAmend'

instance Reiterate (CaseAmend' larger) (z ': zs) where
    reiterate = coerce

-- | for each y in @smaller@, convert it to a (k, v) to insert into the x in @Many larger@
instance (UniqueMemberAt n x larger) => CaseAny (CaseAmend' larger) ((x, y) ': zs) (Int, WrappedAny) where
    caseAny _ v = (i, WrappedAny v)
      where
        i = fromInteger (natVal @n Proxy)

-----------------------------------------------------------------------

-- | A variation of 'amend' which amends via labels.
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar 'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- 'amendL'' \@'[Foo, Bar] Proxy x (\'Y' './' True './' 'ni'l) \`shouldBe`
--     False './' Tagged \@\"Hi" (5 :: Int) './' \'Y' './' True './' Tagged \@\"Bye" \'O' './' 'nil'
-- 'amendL'' \@'[\"Hi", \"Bye"] Proxy x (True './' Tagged \@\"Changed" True './' 'nil') \`shouldBe`
--     False './' True './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Changed" True './' 'nil'
-- @
amendL'
    :: forall ls smaller smaller' larger proxy zipped.
       ( Amend' smaller smaller' larger zipped
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger
       )
    => proxy ls
    -> Many larger
    -> Many smaller'
    -> Many (Replaces smaller smaller' larger)
amendL' _ = amend' @(KindsAtLabels ls larger) Proxy

-----------------------------------------------------------------------
-- | A friendlier type constraint synomyn for 'amendN'
type AmendN ns smaller larger =
    ( AFoldable (CollectorAnyN (CaseAmendN ns larger) 0 smaller) (Int, WrappedAny)
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
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- 'amendN' (Proxy \@'[5, 4, 0]) x (Just \'B' './' (8 :: Int) './' (4 ::Int) './' 'nil') \`shouldBe`
--     (4 :: Int) './' False './' \'X' './' Just \'O' './' (8 :: Int) './' Just \'B' './' 'nil'
-- @
amendN :: forall ns smaller larger proxy.
       (AmendN ns smaller larger)
    => proxy ns -> Many larger -> Many smaller -> Many larger
amendN _ (Many ls) t = Many $ foldr (\(i, WrappedAny v) ys -> S.update i v ys) ls xs'
  where
    xs' = afoldr (:) [] (forManyN' (CaseAmendN @ns @larger @0 @smaller) t)

data CaseAmendN (indices :: [Nat]) (larger :: [Type]) (n :: Nat) (xs :: [Type]) r = CaseAmendN

instance ReiterateN (CaseAmendN indices larger) n (x ': xs) where
    reiterateN = coerce

-- | for each x in @smaller@, convert it to a (k, v) to insert into the x in @larger@
instance (MemberAt n' x larger, n' ~ KindAtIndex n indices) =>
         CaseAny (CaseAmendN indices larger n) (x ': xs) (Int, WrappedAny) where
    caseAny _ v = (i, WrappedAny v)
      where
        i = fromInteger (natVal @n' Proxy)

-----------------------------------------------------------------------

-- | A friendlier type constraint synomyn for 'amendN'
type AmendN' ns smaller smaller' larger zipped =
    ( AFoldable (CollectorAnyN (CaseAmendN' ns larger) 0 zipped) (Int, WrappedAny)
    , smaller ~ KindsAtIndices ns larger
    , IsDistinct ns
    , zipped ~ Zip smaller smaller')

-- | A polymorphic variation of 'amendN'
amendN' :: forall ns smaller smaller' larger proxy zipped.
       (AmendN' ns smaller smaller' larger zipped)
    => proxy ns -> Many larger -> Many smaller' -> Many (ReplacesIndex ns smaller' larger)
amendN' _ (Many ls) t = Many $ foldr (\(i, WrappedAny v) ys -> S.update i v ys) ls xs'
  where
    xs' = afoldr (:) [] (forManyN'' @smaller Proxy (CaseAmendN' @ns @larger @0 @zipped) t)

forManyN'' :: Proxy xs -> c n (Zip xs ys) r -> Many ys -> CollectorAnyN c n (Zip xs ys) r
forManyN'' _ c (Many ys) = CollectorAnyN c (toList ys)

data CaseAmendN' (indices :: [Nat]) (larger :: [Type]) (n :: Nat) (zs :: [Type]) r = CaseAmendN'

instance ReiterateN (CaseAmendN' indices larger) n (z ': zs) where
    reiterateN = coerce

-- | for each x in @smaller@, convert it to a (k, v) to insert into the x in @larger@
instance (MemberAt n' x larger, n' ~ KindAtIndex n indices) =>
         CaseAny (CaseAmendN' indices larger n) ((x, y) ': zs) (Int, WrappedAny) where
    caseAny _ v = (i, WrappedAny v)
      where
        i = fromInteger (natVal @n' Proxy)

-----------------------------------------------------------------------

instance Eq (Many_ '[]) where
    _ == _ = True

instance (Eq x, Eq (Many_ xs)) => Eq (Many_ (x ': xs)) where
    ls == rs = case front' ls == front' rs of
        False -> False
        _ -> (aft' ls) == (aft' rs)
    {-# INLINABLE (==) #-} -- This makes compiling tests a little faster than with no pragma

-- | Two 'Many's are equal if all their fields equal
instance Eq (Many_ xs) => Eq (Many xs) where
    lt == rt = toMany_ lt == toMany_ rt

-----------------------------------------------------------------------

instance Ord (Many_ '[]) where
    compare _ _ = EQ

instance (Ord x, Ord (Many_ xs)) => Ord (Many_ (x ': xs)) where
    compare ls rs = case compare (front' ls) (front' rs) of
        LT -> LT
        GT -> GT
        EQ -> compare (aft' ls) (aft' rs)
    {-# INLINABLE compare #-} -- This makes compiling tests a little faster than with no pragma

-- | Two 'Many's are ordered by 'compare'ing their fields in index order
instance Ord (Many_ xs) => Ord (Many xs) where
    compare xs ys = compare (toMany_ xs) (toMany_ ys)

-----------------------------------------------------------------------

instance Show (Many_ '[]) where
    showsPrec d _ = showParen (d > app_prec) $ showString "nil"
      where
        app_prec = 10

instance (Show x, Show (Many_ xs)) => Show (Many_ (x ': xs)) where
    showsPrec d ls@(Many_ xs) =
        showParen (d > cons_prec) $
        showsPrec (cons_prec + 1) v .
        showString " ./ " .
        showsPrec cons_prec (aft' ls) -- not (cons-prec+1) for right associativity
      where
        cons_prec = 5 -- infixr 5 prefix
        -- use of front here is safe as we are guaranteed the length from the typelist
        v = unsafeCoerce (Partial.head xs) :: x
    {-# INLINABLE showsPrec #-} -- This makes compiling tests a little faster than with no pragma

-- | @show (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil' == "5 ./ False ./ 'X' ./ Just 'O' ./ nil" == @
instance Show (Many_ xs) => Show (Many xs) where
    showsPrec d xs = showsPrec d (toMany_ xs)

-----------------------------------------------------------------------

instance Read (Many_ '[]) where
    readPrec = parens $ prec app_prec $ do
        lift $ L.expect (Ident "nil")
        pure $ Many_ []
      where
        app_prec = 10

instance (Read x, Read (Many_ xs)) => Read (Many_ (x ': xs)) where
    readPrec = parens $ prec cons_prec $ do
        a <- step (readPrec @x)
        lift $ L.expect (Symbol "./")
        as <- readPrec @(Many_ xs) -- no 'step' to allow right associatitive './'
        pure $ prefix' a as
      where
        cons_prec = 5 -- infixr `prefix`
    {-# INLINABLE readPrec #-} -- This makes compiling tests a little faster than with no pragma

-- | @read "5 ./ False ./ 'X' ./ Just 'O' ./ nil" == (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'@
instance Read (Many_ xs) => Read (Many xs) where
    readPrec = do
        xs <- readPrec @(Many_ xs)
        pure $ fromMany_ xs

-- | 'WrappedAny' avoids the following:
-- Illegal type synonym family application in instance: Any
newtype WrappedAny = WrappedAny Any
