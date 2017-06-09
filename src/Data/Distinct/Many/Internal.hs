{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Distinct.Many.Internal where

import Control.Lens
import Data.Distinct.Catalog
import Data.Distinct.TypeLevel
import Data.Kind
import Data.Proxy
import GHC.Prim (Any)
import GHC.TypeLits
import Unsafe.Coerce
import Data.Typeable
import Data.Maybe

import Data.Monoid hiding (Any)

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
-- The functional dependency helps avoid undecidable instances
class Switch xs handlers r | handlers -> r where
    switch :: Many xs -> handlers -> r

-- | A convenient synonym function to create a Catalogs for handling 'switch'.
cases :: (xs ~ TypesOf (Unwrapped (Catalog xs)), Wrapped (Catalog xs)) => Unwrapped (Catalog xs) -> Catalog xs
cases = catalog

-- | Catamorphism for many. This is @flip switch@
many :: Switch xs handlers r => handlers -> Many xs -> r
many = flip switch

-- | This accepts a phantom r to allow 'Catalog' to be used as a type instance for 'Switch'
type Case (xs :: [Type]) r = Catalog xs

instance ( Length xs ~ Length '[a]
         , Has (a -> r) (Case xs r)) => Switch '[a] (Case xs r) r where
    switch (Many _ v) t = (t ^. item) (unsafeCoerce v :: a)

instance ( Length xs ~ Length '[a, b]
         , AllHas (Case xs r) (Accepts r '[a, b])) => Switch '[a, b] (Case xs r) r where
    switch (Many n v) t = case n of
         0 -> (t ^. item) (unsafeCoerce v :: a)
         _ -> (t ^. item) (unsafeCoerce v :: b)

---------------
-- | Holds an existential that can handle any Typeable input
data CaseTypeable r = CaseTypeable (forall a. Typeable a => a -> r)

-- Unfortunately the following doesn't work. GHC isn't able to deduce that (TypeAt x xs) is a Typeable
--
-- instance AllTypeable xs => Switch xs (CaseTypeable r) r where
--     switch (Many n v) (CaseTypeable f) = let Just someNat = someNatVal (toInteger n)
--                                      in case someNat of
--                                             SomeNat (_ :: Proxy x) -> f (unsafeCoerce v :: TypeAt x xs)

instance Typeable a => Switch '[a] (CaseTypeable r) r where
    switch (Many _ v) (CaseTypeable f) = f (unsafeCoerce v :: a)

instance AllTypeable '[a, b] => Switch '[a, b] (CaseTypeable r) r where
    switch (Many n v) (CaseTypeable f) = case n of
         0 -> f (unsafeCoerce v :: a)
         _ -> f (unsafeCoerce v :: b)

-- -- | It is safe to use fromJust as the constructor ensures n is >= 0
-- forany :: forall xs r. Many xs -> (forall a. a -> r) -> r
-- forany (Many n v) f = let someNat = fromJust (someNatVal (toInteger n))
--                       in case someNat of
--                              SomeNat (_ :: Proxy i) ->
--                                  f (unsafeCoerce v :: TypeAt i xs)

-- | Construct a Many out of a value
pick :: forall a xs. (Member a xs) => a -> Many xs
pick = review (facet @a)



-- wacky :: AnyCase (Many xs)
-- wacky = AnyCase pick


-- | A Many has a prism to an the inner type.
class Facet branch tree where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @facet \@Int@
    facet :: Prism' tree branch

-- | UndecidableInstance due to xs appearing more often in the constraint.
-- Safe because xs will not expand to Many xs or bigger.
instance Member a xs => Facet a (Many xs) where
    facet = prism'
        (Many (fromIntegral (natVal @(IndexOf a xs) Proxy)) . unsafeCoerce)
        (\(Many n v) -> if n == fromIntegral (natVal @(IndexOf a xs) Proxy)
            then Just (unsafeCoerce v :: a)
            else Nothing)
    {-# INLINE facet #-}

-- | Injection.
-- Basically the same class as 'Facet' but with type params reversed.
-- A Many can be narrowed to contain more types or have it order changed by injecting into another Many type.
class Inject tree branch where
    -- | Enlarge number of or change order of types in the variant.
    -- Use TypeApplication to specify the destination type.
    -- Example: @inject \@(Many '[Int, String])@
    inject :: Prism' tree branch

-- instance Inject tree (Many branch) where
--     inject = prism' undefined undefined

-- Prism' tree branch
wock :: Prism' String (Last Int)
wock = undefined

weck2 :: String -> (Last Int)
weck2 i = view wock i

weck :: (Last Int) -> String
weck i = review wock i

eck :: String -> Maybe (Last Int)
eck i = preview wock i

ack = re wock

-- weck2 i = preview wock i

-- wack :: forall a. (Many '[a] -> a)
-- wack v = review facet (fromJust (preview (facet) v))

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

-- Create a way to extract value from a 'Many of sized 1 without Maybe

-- Naming: reinterpret_cast, dynamic_cast ?


-- To project from smaller Many to larger Many (always ok)

-- To Inject from larger Many to smaller Many (use Maybe)


-- Show and Read instances

-- disallow empty many

-- FIXME: use type family avoid repeated constraints for each type in xs
