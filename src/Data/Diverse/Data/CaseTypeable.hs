{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Diverse.Data.CaseTypeable where

import Data.Diverse.Class.Case
import Data.Diverse.Class.Reiterate
import Data.Diverse.Type
import Data.Kind
import Data.Typeable

-- | This handler stores a polymorphic function for all Typeables.
newtype CaseTypeable (xs :: [Type]) r = CaseTypeable (forall x. Typeable x => x -> r)

instance Reiterate CaseTypeable xs where
    reiterate (CaseTypeable f) = CaseTypeable f

instance Typeable (Head xs) => Case CaseTypeable xs r where
    then' (CaseTypeable f) = f
