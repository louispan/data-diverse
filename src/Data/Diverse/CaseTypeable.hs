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
newtype CaseTypeable (xs :: [Type]) r = CaseTypeable (forall x. Typeable x => x -> r)

instance Reiterate CaseTypeable xs where
    reiterate (CaseTypeable f) = CaseTypeable f

instance Typeable (Head xs) => Case CaseTypeable xs r where
    case' (CaseTypeable f) = f
