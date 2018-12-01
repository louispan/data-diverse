{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.CaseFunc where

import Data.Diverse.Case
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind

-- | This handler stores a polymorphic function that returns a different type.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y ('CaseFunc' \@'Data.Typeable.Typeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` "Int"
-- @
--
-- @
-- let x = (5 :: Int) 'Data.Diverse.Many../' False 'Data.Diverse.Many../' \'X' 'Data.Diverse.Many../' Just \'O' 'Data.Diverse.Many../' (6 :: Int) 'Data.Diverse.Many../' Just \'A' 'Data.Diverse.Many../' 'Data.Diverse.Many.nul'
-- 'Data.Diverse.AFoldable.afoldr' (:) [] ('Data.Diverse.Many.forMany' ('CaseFunc' \@'Data.Typeable.Typeable' (show . typeRep . (pure @Proxy))) x) \`shouldBe`
--     [\"Int", \"Bool", \"Char", \"Maybe Char", \"Int", \"Maybe Char"]
-- @
newtype CaseFunc (k :: Type -> Constraint) r (xs :: [Type]) = CaseFunc (forall x. k x => x -> r)
-- This was made possible by Syrak
-- https://www.reddit.com/r/haskell/comments/75zrci/help_how_to_pass_constraints_as_a_type_variable/

type instance CaseResult (CaseFunc k r) x = r

instance Reiterate (CaseFunc k r) xs where
    reiterate (CaseFunc f) = CaseFunc f

instance k x => Case (CaseFunc k r) (x ': xs) where
    case' (CaseFunc f) = f

-- | This handler stores a polymorphic function that doesn't change the type.
--
-- @
-- let x = (5 :: Int) 'Data.Diverse.Many../' (6 :: Int8) 'Data.Diverse.Many../' (7 :: Int16) 'Data.Diverse.Many../' (8 :: Int32) 'Data.Diverse.Many../' 'Data.Diverse.Many.nil'
--     y = (15 :: Int) 'Data.Diverse.Many../' (16 :: Int8) 'Data.Diverse.Many../' (17 :: Int16) 'Data.Diverse.Many../' (18 :: Int32) 'Data.Diverse.Many../' 'Data.Diverse.Many.nil'
-- 'Data.Diverse.AFunctor.afmap' ('CaseFunc'' \@'Num' (+10)) x \`shouldBe` y
-- @
newtype CaseFunc' (k :: Type -> Constraint) (xs :: [Type]) = CaseFunc' (forall x. k x => x -> x)

type instance CaseResult (CaseFunc' k) x = x

instance Reiterate (CaseFunc' k) xs where
    reiterate (CaseFunc' f) = CaseFunc' f

instance k x => Case (CaseFunc' k) (x ': xs) where
    case' (CaseFunc' f) = f

-- | This handler stores a polymorphic function that work on higher kinds, eg 'Functor'
-- You may want to use @C0 for @k@
newtype CaseFunc1 (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) r (xs :: [Type]) = CaseFunc1 (forall f x. (k (f x), k1 f, k0 x) => f x -> f r)

type instance CaseResult (CaseFunc1 k k1 k0 r) (f x) = f r

instance Reiterate (CaseFunc1 k k1 k0 r) xs where
    reiterate (CaseFunc1 f) = CaseFunc1 f

instance (k (f x), k1 f, k0 x) => Case (CaseFunc1 k k1 k0 r) (f x ': xs) where
    case' (CaseFunc1 f) = f

newtype CaseFunc1_ (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) r x (xs :: [Type]) = CaseFunc1_ (forall f. (k (f x), k1 f, k0 x) => f x -> f r)

type instance CaseResult (CaseFunc1_ k k1 k0 r x) (f x) = f r

instance Reiterate (CaseFunc1_ k k1 k0 r x) xs where
    reiterate (CaseFunc1_ f) = CaseFunc1_ f

instance (k (f x), k1 f, k0 x) => Case (CaseFunc1_ k k1 k0 r x) (f x ': xs) where
    case' (CaseFunc1_ f) = f


-- | A varation of 'CaseFunc1' that doesn't change the return type
newtype CaseFunc1' (k :: Type -> Constraint) (k1 :: (Type -> Type) -> Constraint) (k0 :: Type -> Constraint) (xs :: [Type]) = CaseFunc1' (forall f x. (k (f x), k1 f, k0 x) => f x -> f x)

type instance CaseResult (CaseFunc1' k k1 k0) (f x) = f x

instance Reiterate (CaseFunc1' k k1 k0) xs where
    reiterate (CaseFunc1' f) = CaseFunc1' f

instance (k (f x), k1 f, k0 x) => Case (CaseFunc1' k k1 k0) (f x ': xs) where
    case' (CaseFunc1' f) = f
