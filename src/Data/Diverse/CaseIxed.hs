{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.CaseIxed where

import Data.Diverse.Case
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind

-- | This handler stores a polymorphic function which changes the type of the containers.
--
-- >>> let f (x :: f a) = Const @String @a $ show x
--
-- >>> let xs = (Just @Int 5) ./ Right @Int False ./ "X" ./ (Left @Int @Bool 6) ./ nil
--
-- >>> afmap (CasedIxedFunc @Show f) xs :: Many '[Const String Int, Const String Bool, Const String Char, Const String Bool]
-- Const "Just 5" ./ Const "Right False" ./ Const "\"X\"" ./ Const "Left 6" ./ nil
--
-- >>> atraverse (CasedIxedFunc @Show f) xs :: Const String (Many '[Int, Bool, Char, Bool])
-- Const "Just 5Right False\"X\"Left 6"
newtype CaseIxedFunc (k :: Type -> Constraint) r (xs :: [Type]) = CaseIxedFunc (forall f x. k (f x) => f x -> r x)

type instance CaseResult (CaseIxedFunc k r) (f x) = r x

instance Reiterate (CaseIxedFunc k r) xs where
    reiterate (CaseIxedFunc f) = CaseIxedFunc f

instance k (f x) => Case (CaseIxedFunc k r) (f x ': xs) where
    case' (CaseIxedFunc f) = f

-- | A variant of 'CaseIxedFunc' for which the type of both containers is fixed.
newtype CaseIxedFunc_ (k :: Type -> Constraint) f r (xs :: [Type]) = CaseIxedFunc_ (forall x. k (f x) => f x -> r x)

type instance CaseResult (CaseIxedFunc_ k f r) (f x) = r x

instance Reiterate (CaseIxedFunc_ k f r) xs where
    reiterate (CaseIxedFunc_ f) = CaseIxedFunc_ f

instance k (f x) => Case (CaseIxedFunc_ k f r) (f x ': xs) where
    case' (CaseIxedFunc_ f) = f

-- | A variant of 'CaseIxedFunc' with more constraints.
--
-- >>> let xs = (Just @Int 5) ./ Right @Int False ./ "X" ./ (Left @Int @Bool 6) ./ nil
--
-- >>> afmap (CaseIxedFunc1 @C0 @Foldable @C0 toList) xs
-- [5] ./ [False] ./ "X" ./ [] ./ nil
--
-- >>> atraverse (CaseIxedFunc1 @C0 @Foldable @C0 toList) xs
-- []
--
-- >>> let ys = (Just @Int 5) ./ Right @Int False ./ "XYZ" ./ nil
--
-- >>> atraverse (CaseIxedFunc1 @C0 @Foldable @C0 toList) ys
-- [5 ./ False ./ 'X' ./ nil,5 ./ False ./ 'Y' ./ nil,5 ./ False ./ 'Z' ./ nil]
newtype CaseIxedFunc1 (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) r (xs :: [Type]) = CaseIxedFunc1 (forall f x. (k (f x), k1 f, k0 x) => f x -> r x)

type instance CaseResult (CaseIxedFunc1 k k1 k0 r) (f x) = r x

instance Reiterate (CaseIxedFunc1 k k1 k0 r) xs where
    reiterate (CaseIxedFunc1 f) = CaseIxedFunc1 f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedFunc1 k k1 k0 r) (f x ': xs) where
    case' (CaseIxedFunc1 f) = f

-- | A variant of 'CaseIxedFunc1' for which the type of both containers is fixed.
newtype CaseIxedFunc1_ (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) f r (xs :: [Type]) = CaseIxedFunc1_ (forall x. (k (f x), k1 f, k0 x) => f x -> r x)

type instance CaseResult (CaseIxedFunc1_ k k1 k0 f r) (f x) = r x

instance Reiterate (CaseIxedFunc1_ k k1 k0 f r) xs where
    reiterate (CaseIxedFunc1_ f) = CaseIxedFunc1_ f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedFunc1_ k k1 k0 f r) (f x ': xs) where
    case' (CaseIxedFunc1_ f) = f


-- | A variant of 'CaseIxedFunc' which maps containers within an additional layer.
--
-- >>> let f (x :: f a) = Const @String @a $ show x
--
-- >>> let xs = (Just @Int 5) ./ Right @Int False ./ "X" ./ (Left @Int @Bool 6) ./ nil
--
-- >>> atraverse (CaseIxedFuncM @Show $ \x -> f x <$ print x) xs
-- Just 5
-- Right False
-- "X"
-- Left 6
-- Const "Just 5" ./ Const "Right False" ./ Const "\"X\"" ./ Const "Left 6" ./ nil
newtype CaseIxedFuncM (k :: Type -> Constraint) r m (xs :: [Type]) = CaseIxedFuncM (forall f x. k (f x) => f x -> m (r x))

type instance CaseResult (CaseIxedFuncM k r m) (f x) = m (r x)

instance Reiterate (CaseIxedFuncM k r m) xs where
    reiterate (CaseIxedFuncM f) = CaseIxedFuncM f

instance k (f x) => Case (CaseIxedFuncM k r m) (f x ': xs) where
    case' (CaseIxedFuncM f) = f

-- | A variant of 'CaseIxedFuncM' for which the type of both containers is fixed.
newtype CaseIxedFuncM_ (k :: Type -> Constraint) f r m (xs :: [Type]) = CaseIxedFuncM_ (forall x. k (f x) => f x -> m (r x))

type instance CaseResult (CaseIxedFuncM_ k f r m) (f x) = m (r x)

instance Reiterate (CaseIxedFuncM_ k f r m) xs where
    reiterate (CaseIxedFuncM_ f) = CaseIxedFuncM_ f

instance k (f x) => Case (CaseIxedFuncM_ k f r m) (f x ': xs) where
    case' (CaseIxedFuncM_ f) = f

-- | A variant of 'CaseIxedFuncM' with more constraints.
--
-- >>> let xs = (Just @Int 5) ./ Right @Int False ./ "XYZ" ./ nil
--
-- >>> atraverse (CaseIxedFuncM1 @C0 @Foldable @C0 @[] @Maybe $ Just . toList) xs
-- Just ([5] ./ [False] ./ "XYZ" ./ nil)
newtype CaseIxedFuncM1 (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) r m (xs :: [Type]) = CaseIxedFuncM1 (forall f x. (k (f x), k1 f, k0 x) => f x -> m (r x))

type instance CaseResult (CaseIxedFuncM1 k k1 k0 r m) (f x) = m (r x)

instance Reiterate (CaseIxedFuncM1 k k1 k0 r m) xs where
    reiterate (CaseIxedFuncM1 f) = CaseIxedFuncM1 f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedFuncM1 k k1 k0 r m) (f x ': xs) where
    case' (CaseIxedFuncM1 f) = f

-- | A variant of 'CaseIxedFuncM1' for which the type of both containers is fixed.
newtype CaseIxedFuncM1_ (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) f r m (xs :: [Type]) = CaseIxedFuncM1_ (forall x. (k (f x), k1 f, k0 x) => f x -> m (r x))

type instance CaseResult (CaseIxedFuncM1_ k k1 k0 f r m) (f x) = m (r x)

instance Reiterate (CaseIxedFuncM1_ k k1 k0 f r m) xs where
    reiterate (CaseIxedFuncM1_ f) = CaseIxedFuncM1_ f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedFuncM1_ k k1 k0 f r m) (f x ': xs) where
    case' (CaseIxedFuncM1_ f) = f


-- | This handler stores a polymorphic function which maps containers to continuations.
--
-- This is especially useful for building 'Data.Diverse.Cases' using 'Data.Diverse.AFunctor.afmap'.
newtype CaseIxedCont (k :: Type -> Constraint) r (xs :: [Type]) = CaseIxedCont (forall f x. k (f x) => f x -> x -> r)

type instance CaseResult (CaseIxedCont k r) (f x) = x -> r

instance Reiterate (CaseIxedCont k r) xs where
    reiterate (CaseIxedCont f) = CaseIxedCont f

instance k (f x) => Case (CaseIxedCont k r) (f x ': xs) where
    case' (CaseIxedCont f) = f

-- | A variant of 'CaseIxedCont' for which the type of both containers is fixed.
--
-- >>> let ps = Predicate @Int (> 5) ./ Predicate isLetter ./ Predicate id ./ nil
--
-- >>> let ps' = cases $ afmap (CaseIxedCont_ @C0 getPredicate) ps
--
-- >>> switch (pick @Int @'[Int, Bool, Char] 5) ps' :: Bool
-- False
--
-- >>> switch (pick @Char @'[Int, Bool, Char] 6) ps' :: Bool
-- True
--
-- >>> switch (pick @Char @'[Int, Bool, Char] '_') ps' :: Bool
-- False
--
-- >>> switch (pick @Int @'[Int, Bool, Char] 'a') ps' :: Bool
-- True
--
-- >>> switch (pick @Bool @'[Int, Bool, Char] False) ps' :: Bool
-- False
--
-- >>> switch (pick @Bool @'[Int, Bool, Char] True) ps' :: Bool
-- True
newtype CaseIxedCont_ (k :: Type -> Constraint) f r (xs :: [Type]) = CaseIxedCont_ (forall x. k (f x) => f x -> x -> r)

type instance CaseResult (CaseIxedCont_ k f r) (f x) = x -> r

instance Reiterate (CaseIxedCont_ k f r) xs where
    reiterate (CaseIxedCont_ f) = CaseIxedCont_ f

instance k (f x) => Case (CaseIxedCont_ k f r) (f x ': xs) where
    case' (CaseIxedCont_ f) = f

-- | A variant of 'CaseIxedCont' with more constraints.
newtype CaseIxedCont1 (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) r (xs :: [Type]) = CaseIxedCont1 (forall f x. (k (f x), k1 f, k0 x) => f x -> x -> r)

type instance CaseResult (CaseIxedCont1 k k1 k0 r) (f x) = x -> r

instance Reiterate (CaseIxedCont1 k k1 k0 r) xs where
    reiterate (CaseIxedCont1 f) = CaseIxedCont1 f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedCont1 k k1 k0 r) (f x ': xs) where
    case' (CaseIxedCont1 f) = f

-- | A variant of 'CaseIxedCont1_' for which the type of both containers is fixed.
newtype CaseIxedCont1_ (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) f r (xs :: [Type]) = CaseIxedCont1_ (forall x. (k (f x), k1 f, k0 x) => f x -> x -> r)

type instance CaseResult (CaseIxedCont1_ k k1 k0 f r) (f x) = x -> r

instance Reiterate (CaseIxedCont1_ k k1 k0 f r) xs where
    reiterate (CaseIxedCont1_ f) = CaseIxedCont1_ f

instance (k (f x), k1 f, k0 x) => Case (CaseIxedCont1_ k k1 k0 f r) (f x ': xs) where
    case' (CaseIxedCont1_ f) = f