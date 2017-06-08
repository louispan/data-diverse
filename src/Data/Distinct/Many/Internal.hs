{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Data.Distinct.Many.Internal where

import Control.Lens
import Data.Distinct.Catalog
import Data.Distinct.TypeLevel
import Data.Kind
import GHC.Prim (Any)
import Unsafe.Coerce

-- | A polymorphic variant or co-record where there are no duplicates in the type list of possible types.
-- This means TypeApplication (instead of labels) can be used to index the variant.
-- This is essentially a typed version of 'Data.Dynamic'
--
-- The variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as Haskus.Util.Many and HList (which used Int instead of Word)
-- See https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
-- With the following differences:
-- * No duplicate types allowed in the type list
-- * don't exposing an indexByN inteface or getByLabel interface
-- * just use TypeApplication for the expected type instead
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Word Any

-- | As per Haskus and HList versions, the inferred role is phantom, which is not safe
type role Many representational

-- -- | Catamorphism for many. Apply a 'Catalog' of functions to a variant of values.
-- class FoldMany xs handlers | xs handlers where
--     many' :: handlers -> Many xs -> r

-- instance Has (a -> r) (t r) => FoldMany '[a] (t r) r where
--     many' t (Many _ v) = (t ^. item) (unsafeCoerce v :: a)

-- instance ( Has (a -> r) (t r)
--          , Has (b -> r) (t r)) => FoldMany '[a, b] (t r) r where
--     many' t (Many n v) = case n of
--         0 -> (t ^. item) (unsafeCoerce v :: a)
--         _ -> (t ^. item) (unsafeCoerce v :: b)

-- instance ( Has (a -> r) (t r)
--          , Has (b -> r) (t r)
--          , Has (c -> r) (t r)) => FoldMany '[a, b, c] (t r) r where
--     many' t (Many n v) = case n of
--         0 -> (t ^. item) (unsafeCoerce v :: a)
--         1 -> (t ^. item) (unsafeCoerce v :: b)
--         _ -> (t ^. item) (unsafeCoerce v :: c)

-- | Catamorphism for many. Apply a 'Catalog' of functions to a variant of values.
class FoldMany xs handlers where
    many :: Catalog handlers -> Many xs -> AcceptResult handlers

instance Has (a -> AcceptResult handlers) (Catalog handlers) => FoldMany '[a] handlers where
    many t (Many _ v) = (t ^. item) (unsafeCoerce v :: a)

instance ( Has (a -> AcceptResult handlers) (Catalog handlers)
         , Has (b -> AcceptResult handlers) (Catalog handlers)) => FoldMany '[a, b] handlers where
    many t (Many n v) = case n of
        0 -> (t ^. item) (unsafeCoerce v :: a)
        _ -> (t ^. item) (unsafeCoerce v :: b)

instance ( Has (a -> AcceptResult handlers) (Catalog handlers)
         , Has (b -> AcceptResult handlers) (Catalog handlers)
         , Has (c -> AcceptResult handlers) (Catalog handlers)) => FoldMany '[a, b, c] handlers where
    many t (Many n v) = case n of
        0 -> (t ^. item) (unsafeCoerce v :: a)
        1 -> (t ^. item) (unsafeCoerce v :: b)
        _ -> (t ^. item) (unsafeCoerce v :: c)

switch :: FoldMany xs handlers => Many xs -> Catalog handlers -> AcceptResult handlers
switch = flip many

-- many :: Switch xs handlers r =>
-- catamorphism of Many
-- many = flip switch

-- -- | Get the index of the variant
-- index :: Many a -> Word
-- index (Many n _) = n

-- TODO:

-- Naming: reinterpret_cast, dynamic_cast ?


-- To have prism from Many to inner type
-- This also allows safe construction.

-- To project from larger Many to smaller Many

-- Show and Read instances

-- disallow empty many



-- -- This encoding uses a data family to allow different sets of GADT constructors,
-- -- with different number of constuctors in each set.
-- -- GADTs are used to ensure the 'Distinct' constraint is met.
-- data family Many (xs :: [*])

-- data instance Many '[] = M0
--     deriving (Eq, Show, Ord, G.Generic)
-- newtype instance Many '[a] = M1 a
--     deriving (Eq, Show, Ord, G.Generic)
-- data instance Many '[a, b] where
--     M2_1 :: Distinct '[a, b] => a -> Many '[a, b]
--     M2_2 :: Distinct '[a, b] => b -> Many '[a, b]
-- data instance Many '[a, b, c] where
--     M3_1 :: Distinct '[a, b, c] => a -> Many '[a, b, c]
--     M3_2 :: Distinct '[a, b, c] => b -> Many '[a, b, c]
--     M3_3 :: Distinct '[a, b, c] => c -> Many '[a, b, c]

-- deriving instance (Eq a, Eq b) => Eq (Many '[a, b])
-- deriving instance (Ord a, Ord b) => Ord (Many '[a, b])
-- deriving instance (Show a, Show b) => Show (Many '[a, b])
-- deriving instance (Distinct '[a, b], Read a, Read b) => Read (Many '[a, b])
-- deriving instance (Eq a, Eq b, Eq c) => Eq (Many '[a, b, c])
-- deriving instance (Ord a, Ord b, Ord c) => Ord (Many '[a, b, c])
-- deriving instance (Show a, Show b, Show c) => Show (Many '[a, b, c])
-- deriving instance (Distinct '[a, b, c], Read a, Read b, Read c) => Read (Many '[a, b, c])
