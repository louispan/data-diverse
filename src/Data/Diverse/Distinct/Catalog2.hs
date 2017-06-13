{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}


module Data.Diverse.Distinct.Catalog2 where

import qualified Data.Map.Strict as M
import GHC.Prim (Any)
import Data.Kind
import Data.Diverse.TypeLevel

-- TODO: Implement Data.Indistinct.Tuple with getByIndex only semantics
-- TODO: Implement Data.Indistinct.Variant with getByIndex only semantics
-- TODO: Implement Data.Labelled.Record with getByLabel only semantics, where fields may only contain tagged values called :=
-- TODO: Implement Data.Labelled.Corecord with getByLabel only semantics, where fields may only contain tagged values called :=

-- | A Catalog is an anonymous product type (also know as polymorphic record), that has fields of distinct types.
-- That is, there are no duplicates types in the fields of the record.
-- This means labels are not required, since the type itself (with type annotations or -XTypeApplications)
-- can be used to get and set fields in the Catalog.
-- This encoding stores the fields as Any in a Map, where the key is index offset of the type in the typelist.
-- THe constructor will guarantee the correct number and types of the elements.
newtype Catalog (xs :: [Type]) = Catalog (M.Map Word Any)

-- | Just like Haskus and HList version which is incorrect
type role Catalog representational

push :: Distinct xs => Catalog xs -> y -> Catalog (Concat xs '[y])
push = undefined

append :: Catalog xs -> Catalog ys -> Catalog (Concat xs ys)
append = undefined
