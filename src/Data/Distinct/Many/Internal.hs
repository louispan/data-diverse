{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}

module Data.Distinct.Many.Internal where

import GHC.Prim (Any)

-- | Many type based on https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
-- With the following differences:
-- * don't exposing an indexByN inteface - only labels are allowed
-- * I am not allowing duplicate types in the type list
-- * Using the singleton library for the type operators.
--
-- A variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as Haskus.Util.Many and HList (which used Int instead of Word)
data Many (l :: [*]) = Many {-# UNPACK #-} !Word Any

-- | As per Haskus and HList versions, the inferred role is phantom, which is not safe
type role Many representational
