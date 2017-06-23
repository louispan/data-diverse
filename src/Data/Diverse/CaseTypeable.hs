{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Diverse.CaseTypeable where

import Data.Diverse.Case
import Data.Diverse.Reiterate
import Data.Diverse.Type
import Data.Kind
import Data.Typeable

-- | This handler stores a polymorphic function for all Typeables.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y ('CaseTypeable' (show . typeRep . (pure \@Proxy))) `shouldBe` "Int"
-- @
--
-- @
-- let x = (5 :: Int) 'Data.Diverse.Many../' False 'Data.Diverse.Many../' \'X' 'Data.Diverse.Many../' Just \'O' 'Data.Diverse.Many../' (6 :: Int) 'Data.Diverse.Many../' Just \'A' 'Data.Diverse.Many../' 'Data.Diverse.Many.nul'
-- 'Data.Diverse.AFoldable.afoldr' (:) [] ('Data.Diverse.Many.forMany' ('CaseTypeable' (show . typeRep . (pure @Proxy))) x) `shouldBe`
--     [\"Int", \"Bool", \"Char", \"Maybe Char", \"Int", \"Maybe Char"]
-- @
newtype CaseTypeable (xs :: [Type]) r = CaseTypeable (forall x. Typeable x => x -> r)

instance Reiterate CaseTypeable xs where
    reiterate (CaseTypeable f) = CaseTypeable f

instance Typeable (Head xs) => Case CaseTypeable xs r where
    case' (CaseTypeable f) = f
