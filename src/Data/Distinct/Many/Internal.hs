{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.Many.Internal where

import GHC.Prim (Any)
import Prelude hiding (head)
import Unsafe.Coerce

-- | Variant type based on https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
-- With the following differences:
-- * No duplicate types in the type list
-- * don't exposing an indexByN inteface or getByLabel interface
-- * just use TypeApplication of the expected type instead
--
-- The variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as Haskus.Util.Many and HList (which used Int instead of Word)
data Many (xs :: [*]) = Many {-# UNPACK #-} !Word Any

-- | As per Haskus and HList versions, the inferred role is phantom, which is not safe
type role Many representational

-- | Get the index of the variant
index :: Many a -> Word
index (Many n _) = n

-- TODO:
-- To have prism from Many to inner type
-- This also allows safe construction.

-- To project from larger Many to smaller Many

-- Show and Read instances

-- disallow empty many

-- -- | Set the first matching type of a Variant
-- getVariant :: forall a l.
--    ( Member a l
--    ) => Many xs -> Maybe a
-- {-# INLINE getVariant #-}
-- getVariant = getVariantN @(IndexOf a l)
