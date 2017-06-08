{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Distinct.Many.Internal where

import Control.Lens
import Data.Distinct.Catalog
import Data.Distinct.TypeLevel
import Data.Kind
import GHC.Prim (Any)
import GHC.TypeLits
import Unsafe.Coerce
import Data.Proxy

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

-- | A switch/case statement for Many. Apply a 'Catalog' of functions to a variant of values.
class Switch xs handlers r | handlers -> r where
    switch :: Many xs -> handlers -> r

type Switcher (xs :: [Type]) r = Catalog xs

instance (Has (a -> r) (Switcher xs r)) => Switch '[a] (Switcher xs r) r where
    switch (Many _ v) t = (t ^. item) (unsafeCoerce v :: a)

instance ( Has (a -> r) (t r)
         , Has (b -> r) (t r)) => Switch '[a, b] (t r) r where
    switch (Many n v) t = case n of
         0 -> (t ^. item) (unsafeCoerce v :: a)
         _ -> (t ^. item) (unsafeCoerce v :: b)

-- | Catamorphism for many. Apply a 'Catalog' of functions to a variant of values.
many :: Switch xs handlers r => handlers -> Many xs -> r
many = flip switch

-- | A Many has a prism to an the inner type.
class Facet value from where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @facet \@Int@
    facet :: Prism' from value

-- | UndecidableInstance due to xs appearing more often in the constraint.
-- Safe because xs will not expand to Many xs or bigger.
instance (KnownNat (IndexOf a xs)) => Facet a (Many xs) where
    facet = prism'
        (Many (fromIntegral (natVal @(IndexOf a xs) Proxy)) . unsafeCoerce)
        (\(Many n v) -> if n == fromIntegral (natVal @(IndexOf a xs) Proxy)
            then Just (unsafeCoerce v :: a)
            else Nothing)
    {-# INLINE facet #-}


-- instance IsSubSet smaller larger => Project (Many smaller) (Many larger) where
--     project = lens

-- wack :: Many larger -> Many smaller


-- | Utilites

-- -- | AllowAmbiguousTypes!
-- natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
-- natValue = fromIntegral (natVal (Proxy :: Proxy n))
-- {-# INLINE natValue #-}

-- check :: Bool -> a -> Maybe a
-- check p a = if p then Just a else Nothing

-- TODO:

-- Naming: reinterpret_cast, dynamic_cast ?


-- To project from smaller Many to larger Many (always ok)

-- To Inject from larger Many to smaller Many (use Maybe)


-- Show and Read instances

-- disallow empty many
