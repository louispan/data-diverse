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
{-# LANGUAGE PolyKinds #-}

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
-- Mnemonic: It doesn't not one of Any type, it contains one of of Many types.
--
-- The variant contains a value whose type is at the given position in the type list.
-- This is similar to the encoding as Haskus.Util.Many and HList (which used Int instead of Word)
-- See https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Many.hs
-- and https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Many.html
--
-- This encoding has the following differences:
-- * No duplicate types allowed in the type list
-- * Labels are not required, just use TypeApplication for the expected type.
-- * Not exposing the type index position, so there is not getByIndex api.
-- Not using GADTs with the Distinct constraint as it gets in the way when I know something is Distinct,
-- but I don't know how to prove it to GHC. Eg a subset of something Distinct is also Distinct...
data Many (xs :: [Type]) = Many {-# UNPACK #-} !Word Any
-- data Many (xs :: [Type]) where
--     Many :: (Distinct xs) => {-# UNPACK #-} !Word -> Any -> Many xs

-- | Unlike Haskus and HList versions, nominal is required for GADTs with constraints
type role Many representational

-- | A switch/case statement for Many.
-- There is only one instance of this class which visits through the possibilities in Many,
-- delegating work to 'CaseMany', ensuring termination when Many only contains one type.
-- Uses 'Case' instances like 'Cases' to apply a 'Catalog' of functions to a variant of values.
-- Or 'CaseTypeable' to apply a polymorphic function that work on all 'Typeables'.
class Switch xs handler r where
    switch :: Many xs -> handler xs r -> r

instance (Case p '[x] r) => Switch '[x] p r where
    switch v p = case notMany v of
            a -> accept p a

instance (Case p (x ': x' ': xs) r, Switch (x' ': xs) p r) =>
         Switch (x ': x' ': xs) p r where
    switch v p =
        case pickHead v of
            Right a -> accept p a
            Left v' -> switch v' (next p)

-- | Allows storing polymorphic functions with extra constraints that
-- is used on each iteration of 'Switch'
-- What is the Visitor pattern doing here?
class Case p xs r where
    accept :: p xs r -> (Head xs -> r)
    next :: p xs r -> p (Tail xs) r

newtype Cases fs (xs :: [Type]) r = Cases (Catalog fs)

-- | Create Cases for handling 'switch' from a tuple.
-- Example: @switch a $ cases (f, g, h)@
-- FIXME: Add additional constraints on return type Accept r
cases :: (fs ~ TypesOf (Unwrapped (Catalog fs)), Wrapped (Catalog fs)) => Unwrapped (Catalog fs) -> Cases fs xs r
cases = Cases . catalog

-- | Uses a phantom xs in order for Case instances to carry additional constraints
data CaseTypeable (xs :: [Type]) r = CaseTypeable (forall a. Typeable a => a -> r)

instance Typeable (Head xs) => Case CaseTypeable xs r where
    accept (CaseTypeable f) = f
    next (CaseTypeable f) = CaseTypeable f

instance (Has (Head xs -> r) (Catalog fs)) => Case (Cases fs) xs r where
    accept (Cases catalog) = catalog ^. item
    next (Cases catalog) = Cases catalog

-- | Catamorphism for many. This is @flip switch@
many :: Switch xs handler r => handler xs r -> Many xs -> r
many = flip switch

-- -- | FIXME: Implement in termns of ForMany and AcceptMany instead
-- instance ( Length xs ~ Length '[a]
--          , Has (a -> r) (Case xs r)) => Switch '[a] (Case xs r) r where
--     switch (Many _ v) t = (t ^. item) (unsafeCoerce v :: a)

-- instance ( Length xs ~ Length '[a, b]
--          , AllHas (Case xs r) (Accepts r '[a, b])) => Switch '[a, b] (Case xs r) r where
--     switch (Many n v) t = case n of
--          0 -> (t ^. item) (unsafeCoerce v :: a)
--          _ -> (t ^. item) (unsafeCoerce v :: b)

---------------


-- -- | Holds an existential that can handle any Typeable input
-- data CaseTypeable r = CaseTypeable (forall a. Typeable a => a -> r)

-- instance Typeable a => Switch '[a] (CaseTypeable r) r where
--     switch (Many _ v) (CaseTypeable f) = f (unsafeCoerce v :: a)

-- instance AllTypeable '[a, b] => Switch '[a, b] (CaseTypeable r) r where
--     switch (Many n v) (CaseTypeable f) = case n of
--          0 -> f (unsafeCoerce v :: a)
--          _ -> f (unsafeCoerce v :: b)


-- class ForMany xs where
--     forMany :: Many xs -> (forall x. Typeable x => x -> r) -> r

-- instance (Typeable x) => ForMany '[x] where
--     forMany v f = case notMany v of
--             a -> f a

-- instance (ForMany (x' ': xs), Typeable x) =>
--          ForMany (x ': x' ': xs) where
--     forMany v f =
--         case pickHead v of
--             Right a -> f a
--             Left v' -> forMany v' f



----------------

-- FIXME: Naming
-- Copied from https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Variant.hs#L363
-- This will be efficiently compiled into a single case statement in 8.2.1
-- See http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html
class Increase xs ys where
    increase :: Many xs -> Many ys

instance (Member x ys, Distinct ys) => Increase '[x] ys where
    increase v = case notMany v of
            a -> toMany a

instance forall x x' xs ys.
      ( Increase (x' ': xs) ys
      , Member x ys
      , Distinct ys
      ) => Increase (x ': x' ': xs) ys
   where
      increase v = case pickHead v of
         Right a  -> toMany a
         Left  v' -> increase v'

class Decrease xs ys where
    decrease :: Many xs -> Maybe (Many ys) -- FIXME: Use Either

instance (KnownNat (PositionOf x ys), Distinct ys) => Decrease '[x] ys where
    decrease v = case notMany v of
        a -> case fromIntegral (natVal @(PositionOf x ys) Proxy) of
                0 -> Nothing
                i -> Just $ Many (i - 1) (unsafeCoerce a)

instance forall x x' xs ys.
      ( Decrease (x' ': xs) ys
      , KnownNat (PositionOf x ys)
      , Distinct ys
      ) => Decrease (x ': x' ': xs) ys
   where
      decrease v = case pickHead v of
         Right a  -> case fromIntegral (natVal @(PositionOf x ys) Proxy) of
                         0 -> Nothing
                         i -> Just $ Many (i - 1) (unsafeCoerce a)
         Left  v' -> decrease v'

-- increase :: Many as -> Many ys
-- increase a = case pickHead a of
--     Right v -> undefined
--     Left v -> undefined -- increase v

proxy2 :: a -> Proxy a
proxy2 _ = Proxy

-- increase :: forall xs ys. (Distinct ys, AllMemberCtx ys ys) => Many xs -> Many ys
-- increase a = case pickHead a of
--     Right v -> case proxy2 v of
--                    (_ :: Proxy v') -> Many (fromIntegral (natVal @(IndexOf v' xs) Proxy)) (unsafeCoerce v)
--     Left v -> undefined --increase v

-- increase2 :: (Distinct ys, AllMemberCtx ys ys, AllMemberCtx xs xs) => Many xs -> Many ys
-- increase2 a = case pickHead a of
--     Right v -> toMany v
--     Left v -> error "hi"

-- | Construct a Many out of a value
toMany :: forall x xs. (Distinct xs, Member x xs) => x -> Many xs
toMany = Many (fromIntegral (natVal @(IndexOf x xs) Proxy)) . unsafeCoerce

-- | Deconstruct a Many into a Maybe value
fromMany :: forall x xs. (Member x xs) => Many xs -> Maybe x
fromMany (Many n v) = if n == fromIntegral (natVal @(IndexOf x xs) Proxy)
            then Just (unsafeCoerce v)
            else Nothing

-- | Try to pick a value out of a Many, and get Either the Right value or the Left-over possibilities.
pick
    :: forall x xs.
       (Member x xs)
    => Many xs -> Either (Many (Without x xs)) x
pick (Many n v) = let i = fromIntegral (natVal @(IndexOf x xs) Proxy)
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Many (n - 1) v)
                          else Left (Many n v)

-- | Pick the first type in the type list.
pickHead :: Many (x ': xs) -> Either (Many xs) x
pickHead (Many n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Many (n - 1) v)

-- pickHead
--     :: forall xs t h. ( Member h xs
--        -- , t ~ (Without h xs)
--        , t ~ Tail xs
--        , h ~ Head xs
--        )
--     => Many xs -> Either (Many t) h
-- pickHead (Many n v) = let i = fromIntegral (natVal @(IndexOf (Head xs) xs) Proxy)
--                       in if n == i
--                          then Right (unsafeCoerce v)
--                          else if n > i
--                               then Left (Many (n - 1) v)
--                               else Left (Many n v)

-- | A Many with one type is not many at all.
-- We can retrieve the value without a Maybe
notMany :: Many '[a] -> a
notMany (Many _ v) = unsafeCoerce v

-- class Diverge xs ys where
--     diverge :: Many xs -> Many ys

-- more :: forall xs ys. (Distinct xs, Distinct ys, Subset xs ys xs) => Many xs -> Many ys

-- http://hsyl20.fr/home/posts/2016-12-12-control-flow-in-haskell-part-2.html

-- | A Many has a prism to an the inner type.
class Facet branch tree where
    -- | Use TypeApplication to specify the destination type of the lens.
    -- Example: @facet \@Int@
    facet :: Prism' tree branch

-- | UndecidableInstance due to xs appearing more often in the constraint.
-- Safe because xs will not expand to @Many xs@ or bigger.
instance (Distinct xs, Member a xs) => Facet a (Many xs) where
    facet = prism' toMany fromMany
    {-# INLINE facet #-}

-- | Injection.
-- A Many can be narrowed to contain more types or have it order changed by injecting into another Many type.
-- This typeclass looks like 'Facet' but is used for different purposes. Also it has the type params reversed.
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



-- more :: forall xs ys. Distinct ys, Subset xs ys xs) => Many xs -> Many ys
-- more = forany toMany

-- -- | Not working as GHC does not know that i is within our range!
-- more :: forall xs ys. (Distinct xs, Distinct ys, Subset xs ys xs) => Many xs -> Many ys
-- more (Many n v) =
--     let someNat = fromJust (someNatVal (toInteger n))
--     in case someNat of
--         SomeNat (_ :: Proxy i) ->
--             let n' = fromIntegral (natVal @(IndexOf (TypeAt i xs) ys) Proxy)
--             in Many n' v

-- more :: forall xs ys. (Distinct ys, Subset xs ys xs) => Many xs -> Many ys
-- more (Many n v) =
--     let someNat = fromJust (someNatVal (toInteger n))
--     in case someNat of
--         SomeNat (_ :: Proxy i) -> toMany' (unsafeCoerce v :: TypeAt i xs)
--   where
--     -- | Doesn't work, GHC cannot instantiate a KnownNat forall x
--     toMany' :: forall x. (Distinct ys, Member x ys) => x -> Many ys
--     toMany' = Many (fromIntegral (natVal @(IndexOf x ys) Proxy)) . unsafeCoerce


-- Unfortunately the following doesn't work. GHC isn't able to deduce that (TypeAt x xs) is a Typeable
-- It is safe to use fromJust as the constructor ensures n is >= 0
-- instance AllTypeable xs => Switch xs (CaseTypeable r) r where
--     switch (Many n v) (CaseTypeable f) = let Just someNat = someNatVal (toInteger n)
--                                      in case someNat of
--                                             SomeNat (_ :: Proxy x) -> f (unsafeCoerce v :: TypeAt x xs)



-- -- | It is safe to use fromJust as the constructor ensures n is >= 0
-- -- Remove as it's not really useful
-- forany :: forall xs r. (forall a. a -> r) -> Many xs -> r
-- forany f (Many n v) =
--     let someNat = fromJust (someNatVal (toInteger n))
--     in case someNat of
--         SomeNat (_ :: Proxy i) ->
--             f (unsafeCoerce v :: TypeAt i xs)
