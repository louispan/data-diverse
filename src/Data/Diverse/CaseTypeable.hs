{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.CaseTypeable where

import Data.Diverse.Case
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind
import Data.Typeable

-- | This handler stores a polymorphic function for all Typeables.
-- This is an example of how to define and instance of 'Case' for
-- polymorphic function into a specified result type.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y ('CaseTypeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` "Int"
-- @
--
-- @
-- let x = (5 :: Int) 'Data.Diverse.Many../' False 'Data.Diverse.Many../' \'X' 'Data.Diverse.Many../' Just \'O' 'Data.Diverse.Many../' (6 :: Int) 'Data.Diverse.Many../' Just \'A' 'Data.Diverse.Many../' 'Data.Diverse.Many.nul'
-- 'Data.Diverse.AFoldable.afoldr' (:) [] ('Data.Diverse.Many.forMany' ('CaseTypeable' (show . typeRep . (pure @Proxy))) x) \`shouldBe`
--     [\"Int", \"Bool", \"Char", \"Maybe Char", \"Int", \"Maybe Char"]
-- @
newtype CaseTypeable r (xs :: [Type]) = CaseTypeable (forall x. Typeable x => x -> r)

type instance CaseResult (CaseTypeable r) x = r

instance Reiterate (CaseTypeable r) xs where
    reiterate (CaseTypeable f) = CaseTypeable f

instance Typeable x => Case (CaseTypeable r) (x ': xs) where
    case' (CaseTypeable f) = f


-- | This handler stores a polymorphic function for all Typeables.
-- This is an example of how to define and instance of 'Case' for
-- polymorphic function that doesn't change the type
newtype CaseTypeable' (xs :: [Type]) = CaseTypeable' (forall x. Typeable x => x -> x)

type instance CaseResult CaseTypeable' x = x

instance Reiterate CaseTypeable' xs where
    reiterate (CaseTypeable' f) = CaseTypeable' f

instance Typeable x => Case CaseTypeable' (x ': xs) where
    case' (CaseTypeable' f) = f
